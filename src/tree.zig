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
        // Represents the root node of the tree as an index into the nodes array. Is null if the tree does not have any nodes.
        root_index: ?usize,
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
        // Edges are stored as internal indices in a 2D resizable array. Every node represented by an index gets its own ArrayList, which contains indices to other nodes. Forming cycles or other types of connections that violate tree properties are not allowed.
        children_indices: ArrayList(ArrayList(usize)),
        // Parent indices are stored seperately, so we do not have to recompute the parent of a node through the children_indices array multiple times.
        parent_indices: ArrayList(?usize),

        // FIXME: Also needs deinit function
        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .root_index = null,
                .nodes = ArrayList(T).init(allocator),
                .children_indices = ArrayList(ArrayList(usize)).init(allocator),
                .parent_indices = ArrayList(?usize).init(allocator),
            };
        }

        // Converts user-supplied node to an index for internal containers.
        // Returns error.InvalidNode if the node is not part of the tree.
        fn toInternalIndex(self: *const Self, node: *const T) !usize {
            const len = self.nodes.items.len;

            // NOTE: If there don't exist any nodes, node can't be inside it.
            if (len == 0) {
                return error.InvalidNode;
            }

            const start = &self.nodes.items[0];
            const end = &self.nodes.items[len - 1];

            if (@intFromPtr(node) < @intFromPtr(start) or @intFromPtr(node) > @intFromPtr(end)) {
                return error.InvalidNode;
            }

            // NOTE: Special case, when we only have one element and are not able to calculate the size of one node for alignment.
            if (len == 1) {
                return if (node == start) 0 else error.InvalidNode;
            }

            const offset = @intFromPtr(node) - @intFromPtr(start);
            // NOTE: We have to use this hack instead of @sizeOf(T) here, as @sizeOf(T) is defined as 0 for comptime types.
            const sizeof = @intFromPtr(&self.nodes.items[1]) - @intFromPtr(start);
            // NOTE: If the pointer is not aligned to the size of an element, it can not be part of the nodes array.
            if (offset % sizeof != 0) {
                return error.InvalidNode;
            }

            const index = offset / sizeof;
            return index;
        }

        fn toExternalNode(self: *const Self, index: usize) *T {
            // TODO: Check that this does not capture a pointer to the stack.
            return &self.nodes.items[index];
        }

        // Returns the root of the tree or null, if the tree is empty.
        pub fn getRoot(self: *const Self) ?*T {
            const r = self.root_index orelse return null;
            return &self.nodes.items[r];
        }

        // Returns whether node is the root of the tree.
        pub fn isRoot(self: *Self, node: *T) !bool {
            const index = try self.toInternalIndex(node);
            const r = self.root_index orelse return false;
            return index == r;
        }

        // Returns whether node is a leaf of the tree.
        pub fn isLeaf(self: *const Self, node: *T) !bool {
            const children = try self.childCountOf(node);
            return children == 0;
        }

        pub fn isDeleted(self: *const Self, node: *T) !bool {
            const index = try self.toInternalIndex(node);

            return switch (self.parent_indices[index]) {
                // NOTE: On deletion, all nodes have their parent node set to null. But this alone can not be used to check for deletion, as the root node also does not have a parent node. This is not a problem, as we store the root index separately and can differentiate between root and deleted node that way.
                null => !self.isRoot(index),
                // NOTE: If we have a parent node stored, then it means that the node can not be deleted.
                _ => false,
            };
        }

        pub fn hasParent(self: *const Self, node: *T) !bool {
            return !self.isRoot(node);
        }

        // Adds a node to the tree and returns the corresponding created entry.
        pub fn addNode(self: *Self, node: T) *const T {
            self.nodes.append(node) catch unreachable;
            self.parent_indices.append(null) catch unreachable;
            self.children_indices.append(ArrayList(usize).init(self.allocator)) catch unreachable;

            const added = &self.nodes.items[self.nodes.items.len - 1];
            if (self.root_index == null) {
                // NOTE: We just added that node, so there is no way this errors out.
                const index = self.toInternalIndex(added) catch unreachable;
                self.root_index = index;
            }

            return added;
        }

        fn removeNode(self: *Self, node: *T) void {
            const index = self.toInternalIndex(node) catch unreachable;

            if (self.root_index) |r| {
                if (r == index) {
                    self.root_index = null;
                }
            }

            self.parent_indices[index] = null;
            self.children_indices.items[index].deinit();
        }

        pub fn removeSubtree(self: *Self, starting_at: *T) !struct { *T, Tree(T) } {
            var subtree = Tree(T).init(self.allocator);
            const sa_parent = try self.parentOf(starting_at);

            var queue = ArrayList(T).init(self.allocator);
            defer queue.deinit();

            var mapping = AutoHashMap(*T, *T).init(self.allocator);
            defer mapping.deinit();

            queue.append(starting_at);

            while (queue.popOrNull()) |node| {
                const added = subtree.addNode(*node);
                mapping.put(node, added);

                const children = try self.childrenOf(node);
                defer children.deinit();

                const parent = try self.parentOf(node);

                if (parent) |p| {
                    if (mapping.get(p)) |sp| {
                        subtree.addEdgeBetween(sp, added);
                    }
                }

                queue.appendSlice(children.items);
                self.removeNode(node);
            }

            return .{ sa_parent, subtree };
        }

        pub fn replaceNodeWith(self: *Self, node: *T, replacement: T) !void {
            const i = try self.toInternalIndex(node);
            self.nodes[i] = replacement;
        }

        // Adds an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If
        // both parent and child are valid nodes, but the new edge would violate
        // tree properties, error.InvalidEdge will be returned.
        // TODO: Maybe rename parent -> from and child -> to, though I don't know if that is clearer.
        pub fn addEdgeBetween(self: *Self, parent: *const T, child: *const T) !void {
            const parent_index = try self.toInternalIndex(parent);
            const child_index = try self.toInternalIndex(child);

            // NOTE: Pointing to a node multiple times is not allowed.
            // This also automatically prevents cycles from being formed.
            if (self.parent_indices.items[child_index] != null) return error.InvalidEdge;

            try self.children_indices.items[parent_index].append(child_index);
        }

        // Removes an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If both
        // parent and child are valid nodes, but there exists no edge between them,
        // error.InvalidEdge will be returned.
        pub fn removeEdgeBetween(self: *Self, parent: *T, child: *T) !void {
            const parent_index = try self.toInternalIndex(parent);
            const child_index = try self.toInternalIndex(child);

            for (self.children_indices.items[parent_index], 0..) |ci, i| {
                if (ci == child_index) {
                    // TODO: Does the order of children matter? If not, this can be a swapRemove instead. If order matters, we also need to improve the insertion behaviour, because right now we can't easily control the order children are inserted in.
                    _ = self.children_indices.items[parent_index].orderedRemove(i);
                    return;
                }
            }

            return error.InvalidEdge;
        }

        /// Returns the parent of node or error.InvalidNode, if node is not
        /// part of the tree.
        pub fn parentOf(self: *Self, node: *T) !?*T {
            const index = try self.toInternalIndex(node);
            const p_index = self.parent_indices[index] orelse return null;
            return self.toExternalNode(p_index);
        }

        // Returns an array containing the children of node, or error.InvalidNode if node is not part of the tree.
        pub fn childrenOf(self: *Self, node: *T) !ArrayList(*T) {
            const index = try self.toInternalIndex(node);
            const child_indices = self.children_indices.items[index];
            const child_nodes = ArrayList(*T).init(self.allocator);

            for (child_indices) |ci| {
                child_nodes.append(self.toExternalNode(ci));
            }

            return child_nodes;
        }

        // Returns the number of children that node has, or error.InvalidNode
        // if node is not part of the tree.
        pub fn childCountOf(self: *const Self, node: *const T) !usize {
            const index = try self.toInternalIndex(node);
            return self.children_indices.items[index].items.len;
        }

        // Returns the nth child of the node, error.InvalidNode if node is
        // not part of the tree, or error.OutOfBounds if node has < n children.
        pub fn nthChild(self: *const Self, node: *T, n: usize) !*T {
            const children = try self.childCountOf(node);
            if (n > children) return error.OutOfBounds;

            const index = try self.toInternalIndex(node);
            return self.toExternalNode(self.children_indices.items[index].items[n]);
        }

        fn firstNondeletedAfter(self: *const Self, index: usize) ?*T {
            for (index..self.nodes.items.len) |i| {
                const node = self.getNthNode(i) catch unreachable;
                if (!self.isDeleted(node)) {
                    return node;
                }
            }

            return null;
        }

        fn swapNodes(self: *Self, a: *T, b: *T) void {
            const a_i = self.toInternalIndex(a) catch unreachable;
            const b_i = self.toInternalIndex(b) catch unreachable;

            const a_parent = self.parent_indices.items[a_i];
            const b_parent = self.parent_indices.items[b_i];

            const a_children = self.children_indices.items[a_i];
            const b_children = self.children_indices.items[b_i];

            self.parent_indices.items[b_i] = a_parent;
            self.parent_indices.items[a_i] = b_parent;

            self.children_indices.items[b_i] = a_children;
            self.children_indices.items[a_i] = b_children;

            self.nodes.items[b_i] = a.*;
            self.nodes.items[a_i] = b.*;
        }

        pub fn shrink(self: *Self) void {
            const deletion_start = blk: for (0..self.nodes.items.len) |i| {
                const node = self.getNthNode(i) catch unreachable;
                if (self.isDeleted(node)) {
                    // NOTE: If there are only deleted nodes after this, we are done and should move to cleanup.
                    const replacement = self.firstNondeletedAfter(i) orelse break :blk i;

                    self.swapNodes(node, replacement);
                }
            } else {
                break :blk null;
            };

            if (deletion_start) |d| {
                self.nodes.shrinkAndFree(d);
                self.children_indices.shrinkAndFree(d);
                self.parent_indices.shrinkAndFree(d);
            }
        }

        pub fn getNthNode(self: *const Self, n: usize) !*T {
            if (n >= self.nodes.items.len) {
                return error.InvalidIndex;
            }

            return self.nodes.items[n];
        }

        pub fn formatHelper(
            self: Self,
            node: *T,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.isLeaf(node) catch unreachable) {
                try node.format(fmt, options, writer);
                return;
            }

            try writer.writeAll("(");
            try node.format(fmt, options, writer);

            var i: usize = 0;
            const children = self.childCountOf(node) catch unreachable;
            while (i < children) : (i += 1) {
                try writer.writeAll(" ");
                try self.formatHelper(self.nthChild(node, i) catch unreachable, fmt, options, writer);
            }

            try writer.writeAll(")");
        }

        pub fn format(
            self: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.getRoot()) |r| {
                try self.formatHelper(r, fmt, options, writer);
            } else {
                try writer.writeAll("empty");
            }
        }
    };
}
