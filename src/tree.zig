const std = @import("std");

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const Allocator = std.mem.Allocator;

pub const TreeError = error{
    InvalidNode,
    InvalidIndex,
    InvalidEdge,
    InvalidRoot,
    OutOfBounds,
};

pub fn Tree(
    comptime T: type,
) type {
    return struct {
        const Self = @This();

        pub const ConnectedNode = struct {
            parent: ?*T,
            node: *T,
            children: []*T,
        };

        // Allocator for internal containers provided by the user.
        allocator: Allocator,
        // Represents the root node of the tree as an index into the nodes array.
        root_index: usize,
        // Holds every node ever added to the tree, even if some of them were deleted
        // in the past. This is necessary, because removal of nodes from the array would
        // require recomputing all edges and the node index map. So this is a tradeoff between
        // memory consumption and runtime cost. It may also prevent errors caused by dangling pointers
        // that are handed out, since the address of a node will never change after insertion.
        //
        // If the overhead of deleted nodes should become a problem, there is shrink(), which will
        // remove all deleted nodes from the tree and recompute all necessary edges and maps explicitly.
        // It should be noted that all stored references to nodes are considered invalid after this operation.
        nodes: ArrayList(T),
        // Edges are stored as internal indices in a 2D resizable array. Every node represented by an index
        // gets their own ArrayList, which contain the other nodes, to which edges are connected. Forming cycles
        // or other types of connections that violate tree properties are not allowed.
        edges: ArrayList(ArrayList(usize)),
        // Parents are stored seperately as a deduplicated inverse of the edges to speed up tree traversal and
        // other operations.
        parent_indices: ArrayList(?usize),
        // Map of actual nodes to internal indices. Using this additional layer of indirection provides a
        // boundary between external and internal uses of nodes and makes it impossible for deleted nodes
        // or nodes of other trees to be passed off as legitimate.
        deleted_nodes: AutoHashMap(usize, void),

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .root_index = 0,
                .nodes = ArrayList(T).init(allocator),
                .edges = ArrayList(ArrayList(usize)).init(allocator),
                .parent_indices = ArrayList(?usize).init(allocator),
                .deleted_nodes = AutoHashMap(usize, void).init(allocator),
            };
        }

        // Converts user-supplied node to an index for internal containers.
        // Returns error.InvalidNode if the node is not part of the tree.
        fn filterKey(self: *const Self, key: usize) !usize {
            return if (self.deleted_nodes.contains(key) or key >= self.nodes.items.len) error.InvalidNode else key;
        }

        // Returns the root of the tree or null if the tree is empty.
        pub fn root(self: *const Self) ?usize {
            if (self.nodes.items.len == 0) return null;
            return self.root_index;
        }

        pub fn isRoot(self: *Self, node: *T) bool {
            const index = try self.toInternalIndex(node) catch return false;
            return index == self.root_index;
        }

        pub fn isLeaf(self: *const Self, key: usize) bool {
            // Not part of the tree, so automatically a leaf
            const children = self.childCountOf(key) catch return true;
            return children == 0;
        }

        fn isDeleted(self: *const Self, node: usize) bool {
            return self.deleted_nodes.contains(node);
        }

        pub fn setRoot(self: *Self, key: usize) !void {
            const index = try self.filterKey(key);
            if (self.parent_indices.items[index] != null) return error.InvalidRoot;
            self.root_index = index;
        }

        pub fn getNode(self: *const Self, key: usize) !T {
            const index = try self.filterKey(key);
            return self.nodes.items[index];
        }

        // Adds a node to the tree and returns the corresponding created entry.
        pub fn addNode(self: *Self, node: T) usize {
            self.nodes.append(node) catch unreachable;
            self.parent_indices.append(null) catch unreachable;
            self.edges.append(ArrayList(usize).init(self.allocator)) catch unreachable;

            return self.nodes.items.len - 1;
        }

        // Removes a node from the tree by severing all edges with parent and
        // children. They will be returned as part of a ConnectedNode struct,
        // so the caller can decide what to do with them.
        pub fn removeNode(self: *Self, node: *T) !ConnectedNode {
            const parent = try self.parentOf(node);
            const children = try self.childrenOf(node);

            if (parent) |p| {
                self.removeEdgeBetween(p, node);
            }

            for (children) |child| {
                self.removeEdgeBetween(node, child);
            }

            const index = try self.toInternalIndex(node);
            self.parent_indices.items[index] = null;
            self.edges.items[index].deinit();
            self.deleted_nodes.put(node, {});

            return .{
                .parent = parent,
                .node = node,
                .children = children,
            };
        }

        // Adds an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If
        // both parent and child are valid nodes, but the new edge would violate
        // tree properties, error.InvalidEdge will be returned.
        // FIXME: Doesn't actually check for cycles because lazy.
        pub fn addEdgeBetween(self: *Self, parent_key: usize, child_key: usize) !void {
            const parent_index = try self.filterKey(parent_key);
            const child_index = try self.filterKey(child_key);

            if (self.parent_indices.items[child_index] != null) return error.InvalidEdge;

            for (self.edges.items[parent_index].items) |ci| {
                if (ci == child_index) return error.InvalidEdge;
            }

            try self.edges.items[parent_index].append(child_index);
        }

        // Removes an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If both
        // parent and child are valid nodes, but there exists no edge between them,
        // error.InvalidEdge will be returned.
        pub fn removeEdgeBetween(self: *Self, parent: *T, child: *T) !void {
            const parent_index = try self.toInternalIndex(parent);
            const child_index = try self.toInternalIndex(child);

            for (self.edges.items[parent_index]) |ci, i| {
                if (ci == child_index) {
                    _ = self.edges.items[parent_index].orderedRemove(i);
                    return;
                }
            }

            return error.InvalidEdge;
        }

        // Returns the parent of node or error.InvalidNode, if node is not
        // part of the tree.
        pub fn parentOf(self: *Self, node: *T) !?*T {
            const index = try self.toInternalIndex(node);
            return self.toExternalNode(self.parent_indices[index]);
        }

        // TODO: Instead of the next three functions, provide an iterator instead.
        // Returns an array containing the children of node, or error.InvalidNode
        // if node is not part of the tree.
        pub fn childrenOf(self: *Self, key: usize) ![]usize {
            const index = try self.filterKey(key);
            return self.edges.items[index].items;
        }

        // Returns the number of children that node has, or error.InvalidNode
        // if node is not part of the tree.
        pub fn childCountOf(self: *const Self, key: usize) !usize {
            const index = try self.filterKey(key);
            return self.edges.items[index].items.len;
        }

        // Returns the nth child of the node, error.InvalidNode if node is
        // not part of the tree, or error.OutOfBounds if node has < n children.
        pub fn nthChild(self: *const Self, key: usize, n: usize) !usize {
            const children = try self.childCountOf(key);
            if (n > children) return error.OutOfBounds;

            const index = try self.filterKey(key);
            return self.edges.items[index].items[n];
        }

        // pub fn shrink(self: *Self) void; TODO
        // TODO: iterator

        pub fn formatHelper(
            self: Self,
            key: usize,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            const node = self.getNode(key) catch unreachable;
            if (self.isLeaf(key)) {
                try node.format(fmt, options, writer);
                return;
            }

            try writer.writeAll("(");
            try node.format(fmt, options, writer);

            var i: usize = 0;
            const children = self.childCountOf(key) catch unreachable;
            while (i < children) : (i += 1) {
                try writer.writeAll(" ");
                try self.formatHelper(self.nthChild(key, i) catch unreachable, fmt, options, writer);
            }

            try writer.writeAll(")");
        }

        pub fn format(
            self: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.root()) |r| {
                try self.formatHelper(r, fmt, options, writer);
            } else {
                try writer.writeAll("empty");
            }
        }
    };
}
