const std = @import("std");

const ArrayList = std.ArrayList;
const SegmentedList = std.SegmentedList;
const PriorityDeque = std.PriorityDequeue;
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
        pub const Iterator = std.SegmentedList(T, 0).Iterator;
        pub const ConstIterator = std.SegmentedList(T, 0).ConstIterator;

        // Allocator for internal containers provided by the user.
        allocator: Allocator,
        // The root of the tree, of which there can only be one. Is null if the tree does not have any nodes.
        root: ?*T,
        // Holds every node ever added to the tree, even if some of them were deleted in the past. This is necessary, because removal of nodes from the array would require recomputing all edges and the node index map. So this is a tradeoff between memory consumption and runtime cost. To prevent pointers from being invalidated, we use a SegmentedList. If the overhead of deleted nodes should become a problem, there is shrink(), which will remove all deleted nodes from the tree and recompute all necessary edges and maps explicitly. It should be noted that all stored references to nodes are invalidated by this operation.
        nodes: SegmentedList(T, 0),
        // Contains child nodes of every node. Forming cycles or other types of connections that violate tree properties are not allowed.
        children: AutoHashMap(*const T, ArrayList(*const T)),
        // Contains parent of every node.
        parents: AutoHashMap(*const T, ?*const T),

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .root = null,
                .nodes = SegmentedList(T, 0){},
                .children = AutoHashMap(*const T, ArrayList(*const T)).init(allocator),
                .parents = AutoHashMap(*const T, ?*const T).init(allocator),
            };
        }

        pub fn deinit(self: *Self) !void {
            var it = self.nodes.iterator(0);

            while (it.next()) |node| {
                const removed = try self.isRemoved(node);
                if (!removed) {
                    try self.removeNode(node);
                }
            }

            try self.shrink();

            self.nodes.deinit(self.allocator);
            self.children.deinit();
            self.parents.deinit();
        }

        // Returns error.InvalidNode if the node is not part of the tree.
        fn validateNode(self: *const Self, node: *const T) !void {
            if (!self.parents.contains(node)) {
                return error.InvalidNode;
            }
        }

        // Returns the root of the tree or null, if the tree is empty.
        pub fn getRoot(self: *const Self) ?*T {
            return self.root;
        }

        // Returns whether node is the root of the tree.
        pub fn isRoot(self: *const Self, node: *const T) !bool {
            try self.validateNode(node);
            const r = self.root orelse return false;
            return node == r;
        }

        // Returns whether node is a leaf of the tree.
        pub fn isLeaf(self: *const Self, node: *const T) !bool {
            const children = try self.childCountOf(node);
            return children == 0;
        }

        // Returns whether node was removed from the tree.
        // TODO: Rename function and all other occurances from Removed -> Deleted? Maybe Disconnected is better? Deleted is not really accurate, since this function would also hold for added nodes without any connecting edges.
        pub fn isRemoved(self: *const Self, node: *const T) !bool {
            try self.validateNode(node);
            const parent = self.parents.get(node).?;

            if (parent != null) {
                // NOTE: If we have a parent node stored, then it means that the node can not be deleted.
                return false;
            } else {
                // NOTE: On deletion, all nodes have their parent node set to null. But this alone can not be used to check for deletion, as the root node also does not have a parent node. This is not a problem, as we store the root separately and can differentiate between root and deleted node that way.
                return !try self.isRoot(node);
            }
        }

        pub fn hasParent(self: *const Self, node: *const T) !bool {
            try self.validateNode(node);

            const parent = try self.parentOf(node);
            return parent != null;
        }

        // Adds a node to the tree and returns the corresponding created entry.
        pub fn addNode(self: *Self, node: T) !*const T {
            const node_ptr = try self.nodes.addOne(self.allocator);
            node_ptr.* = node;

            try self.parents.put(node_ptr, null);
            try self.children.put(node_ptr, ArrayList(*const T).init(self.allocator));

            if (self.root == null) {
                self.root = node_ptr;
            }

            return node_ptr;
        }

        fn insertionOrder(context: void, a: *const T, b: *const T) std.math.Order {
            _ = context;
            _ = a;
            _ = b;
            return std.math.Order.eq;
        }

        pub fn addSubtree(self: *Self, starting_at: *const T, subtree: *const Tree(T)) !void {
            try self.validateNode(starting_at);

            var queue = PriorityDeque(*const T, void, insertionOrder).init(self.allocator, {});
            defer queue.deinit();

            var mapping = AutoHashMap(*const T, *const T).init(self.allocator);
            defer mapping.deinit();

            if (subtree.getRoot()) |r| {
                try queue.add(r);
            }

            while (queue.removeMinOrNull()) |node| {
                const added = try self.addNode(node.*);
                try mapping.put(node, added);

                const children = try subtree.childrenOf(node);
                defer children.deinit();

                const parent = try subtree.parentOf(node);

                if (parent) |p| {
                    if (mapping.get(p)) |sp| {
                        try self.addEdgeBetween(sp, added);
                    }
                } else {
                    try self.addEdgeBetween(starting_at, added);
                }

                try queue.addSlice(children.items);
            }
        }

        fn removeNode(self: *Self, node: *const T) !void {
            try self.validateNode(node);
            if (try self.isRemoved(node)) {
                // TODO: Separate error type here?
                return error.InvalidNode;
            }

            if (try self.isRoot(node)) {
                self.root = null;
            }

            const parent = (try self.parents.fetchPut(node, null)).?.value;

            if (parent) |p| {
                var children = self.children.getPtr(p).?;
                const index = for (children.items, 0..) |c, i| {
                    if (c == node) break i;
                } else null;

                if (index) |i| {
                    _ = children.orderedRemove(i);
                }
            }
        }

        pub fn removeSubtree(self: *Self, starting_at: *const T) !struct { ?*const T, Tree(T) } {
            var subtree = Tree(T).init(self.allocator);
            const sa_parent = try self.parentOf(starting_at);

            var queue = PriorityDeque(*const T, void, insertionOrder).init(self.allocator, {});
            defer queue.deinit();

            var mapping = AutoHashMap(*const T, *const T).init(self.allocator);
            defer mapping.deinit();

            try queue.add(starting_at);

            while (queue.removeMinOrNull()) |node| {
                const added = try subtree.addNode(node.*);
                try mapping.put(node, added);

                const children = try self.childrenOf(node);
                defer children.deinit();

                const parent = try self.parentOf(node);

                if (parent) |p| {
                    if (mapping.get(p)) |sp| {
                        try subtree.addEdgeBetween(sp, added);
                    }
                }

                try queue.addSlice(children.items);
                try self.removeNode(node);
            }

            return .{ sa_parent, subtree };
        }

        // Adds an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If
        // both parent and child are valid nodes, but the new edge would violate
        // tree properties, error.InvalidEdge will be returned.
        pub fn addEdgeBetween(self: *Self, parent: *const T, child: *const T) !void {
            // FIXME: Remove these methods, now that we store pointers in HashMaps explicitly, error should be instead caught by trying to access the HashMaps.
            try self.validateNode(parent);
            try self.validateNode(child);

            // NOTE: Pointing to a node multiple times is not allowed.
            // This also automatically prevents cycles from being formed.
            // We also do not allow to point to the root node, as the root node is defined as the one node not having a parent node.
            if (self.parents.get(child).? != null or try self.isRoot(child)) {
                return error.InvalidEdge;
            }

            try self.parents.put(child, parent);
            try self.children.getPtr(parent).?.append(child);
        }

        // Removes an edge between parent and child. If either parent or child
        // are not part of the tree, error.InvalidNode will be returned. If both
        // parent and child are valid nodes, but there exists no edge between them,
        // error.InvalidEdge will be returned.
        pub fn removeEdgeBetween(self: *Self, parent: *const T, child: *const T) !void {
            try self.validateNode(parent);
            try self.validateNode(child);

            const current_parent = self.parents.get(child);

            if (current_parent) |p| {
                if (p != parent) {
                    return error.InvalidEdge;
                }
            } else {
                return error.InvalidEdge;
            }

            var children = self.children.getPtr(parent).?;

            const index = for (children.items, 0..) |c, i| {
                if (c == child) {
                    break i;
                }
            } else {
                return error.InvalidEdge;
            };

            _ = children.orderedRemove(index);
            try self.parents.put(child, null);
        }

        /// Returns the parent of node or error.InvalidNode, if node is not
        /// part of the tree.
        pub fn parentOf(self: *const Self, node: *const T) !?*const T {
            try self.validateNode(node);
            return self.parents.get(node).?;
        }

        // Returns an array containing the children of node, or error.InvalidNode if node is not part of the tree.
        pub fn childrenOf(self: *const Self, node: *const T) !ArrayList(*const T) {
            try self.validateNode(node);
            const children = self.children.get(node).?;

            // TODO: Should we do a clone here or return a ConstIterator?
            return children.clone();
        }

        // Returns the number of children that node has, or error.InvalidNode
        // if node is not part of the tree.
        pub fn childCountOf(self: *const Self, node: *const T) !usize {
            try self.validateNode(node);
            return self.children.get(node).?.items.len;
        }

        pub fn shrink(self: *Self) !void {
            // NOTE: SegmentedList only implements pop() for removing elements from the end. So we can not remove elements at arbitrary positions and the best we can do is to construct an entirely new SegmentedList.
            var node_replacement = SegmentedList(T, 0){};

            var it = self.nodes.iterator(0);
            while (it.next()) |node| {
                if (!try self.isRemoved(node)) {
                    const slot = try node_replacement.addOne(self.allocator);
                    slot.* = node.*;

                    // NOTE: Some of this stuff can definitely be done by calling existing functions. There should at least be internal convenience function for getting the parent pointer or child pointer of a node into the parents, children and node datastructures.
                    if (try self.isRoot(node)) {
                        self.root = slot;
                    }

                    const expired_parent = self.parents.fetchRemove(node).?.value;
                    const expired_children = self.children.fetchRemove(node).?.value;

                    if (expired_parent) |ep| {
                        const parent_children = self.children.getPtr(ep).?;
                        for (parent_children.items, 0..) |c, i| {
                            if (c == node) {
                                parent_children.items[i] = slot;
                            }
                        }
                    }

                    for (expired_children.items) |c| {
                        const child_parent = self.parents.getPtr(c).?;
                        if (child_parent.*) |cp| {
                            if (cp == node) {
                                child_parent.* = slot;
                            }
                        }
                    }

                    try self.parents.put(slot, expired_parent);
                    try self.children.put(slot, expired_children);

                    continue;
                }

                _ = self.parents.remove(node);
                const entry = self.children.fetchRemove(node).?;
                entry.value.deinit();
            }

            self.nodes.deinit(self.allocator);
            self.nodes = node_replacement;
        }

        pub fn getNthNode(self: *const Self, n: usize) !*const T {
            if (n >= self.nodes.len) {
                return error.InvalidIndex;
            }

            return self.nodes.at(n);
        }

        fn formatHelper(
            self: Self,
            node: *const T,
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

            const children = self.childrenOf(node) catch unreachable;
            defer children.deinit();

            for (children.items) |child| {
                try writer.writeAll(" ");
                try self.formatHelper(child, fmt, options, writer);
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

        pub fn iterator(self: *Self, start_index: usize) Iterator {
            return self.nodes.iterator(start_index);
        }

        pub fn constIterator(self: *const Self, start_index: usize) ConstIterator {
            return self.nodes.constIterator(start_index);
        }
    };
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;

fn test_init_allocator() GeneralPurposeAllocator(.{}) {
    return GeneralPurposeAllocator(.{}){};
}

fn test_deinit_allocator(gpa: *GeneralPurposeAllocator(.{})) void {
    const deinit_status = gpa.deinit();
    if (deinit_status == .leak) expect(false) catch @panic("leaked memory");
}

test "init and deinit" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;
}

test "childCountOf" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);
    try t.addEdgeBetween(one, four);

    try expect(try t.childCountOf(one) == 3);
    try expect(try t.childCountOf(two) == 0);
    try expect(try t.childCountOf(three) == 0);
    try expect(try t.childCountOf(four) == 0);

    _, var st = try t.removeSubtree(two);
    defer st.deinit() catch unreachable;

    try expect(try t.childCountOf(one) == 2);
}

test "addEdgeBetween cycle" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);
    try expectError(TreeError.InvalidEdge, t.addEdgeBetween(two, three));
}

test "getRoot" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    try expect(t.getRoot() == null);
    const one = try t.addNode(1);
    try expect(t.getRoot().? == one);
    const parent, var subtree = try t.removeSubtree(one);
    defer subtree.deinit() catch unreachable;
    try expect(parent == null);
    // NOTE: Comparing by pointer is obviously not possible, because the subtree is stored at a different memory location. So we need to dereference both sides to verify that the root is correct.
    try expect(subtree.getRoot().?.* == one.*);

    try expect(t.getRoot() == null);
    const two = try t.addNode(2);
    _ = try t.addNode(3);
    try expect(t.getRoot().? == two);
}

test "isRoot" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const garbabe: *usize = @ptrFromInt(0xdeadbee0);
    try expectError(error.InvalidNode, t.isRoot(garbabe));

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);

    try t.addEdgeBetween(one, two);

    try expect(try t.isRoot(one) == true);
    try expect(try t.isRoot(two) == false);
    try expect(try t.isRoot(three) == false);
    _, var subtree = try t.removeSubtree(one);
    defer subtree.deinit() catch unreachable;

    const four = try t.addNode(4);
    try expect(try t.isRoot(three) == false);
    try expect(try t.isRoot(four) == true);
}

test "isLeaf" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);
    try t.addEdgeBetween(two, four);
    try t.addEdgeBetween(two, five);

    try expect(!try t.isLeaf(one));
    try expect(!try t.isLeaf(two));
    try expect(try t.isLeaf(three));
    try expect(try t.isLeaf(four));
    try expect(try t.isLeaf(five));

    _, var s1 = try t.removeSubtree(two);
    defer s1.deinit() catch unreachable;

    _, var s2 = try t.removeSubtree(three);
    defer s2.deinit() catch unreachable;

    try expect(try t.isLeaf(one));
    try expect(try t.isLeaf(three));
}

test "isRemoved" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    try expect(!try t.isRemoved(one));
    try t.removeNode(one);
    try expect(try t.isRemoved(one));
}

test "hasParent" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(two, three);

    try expect(!try t.hasParent(one));
    try expect(try t.hasParent(two));
    try expect(try t.hasParent(three));

    _, var st = try t.removeSubtree(three);
    defer st.deinit() catch unreachable;

    // NOTE: Should we be able to pass removed nodes into this function, even though they are not exactly part of the tree anymore?
    try expect(!try t.hasParent(three));
}

test "addNode" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const nums = [16]usize{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

    for (nums) |n| {
        _ = try t.addNode(n);
    }

    var added = ArrayList(usize).init(allocator);
    defer added.deinit();

    for (0..nums.len) |i| {
        const node = try t.getNthNode(i);
        try added.append(node.*);
    }

    // NOTE: Should we require nodes being returned in insertion order?
    try expect(std.mem.eql(usize, added.items, &nums));
}

test "addSubtree" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t1 = Tree(usize).init(allocator);
    defer t1.deinit() catch unreachable;

    var t2 = Tree(usize).init(allocator);
    defer t2.deinit() catch unreachable;

    const one = try t1.addNode(1);
    const two = try t1.addNode(2);
    const three = try t1.addNode(3);

    try t1.addEdgeBetween(one, two);
    try t1.addEdgeBetween(one, three);

    const four = try t2.addNode(4);
    const five = try t2.addNode(5);
    const six = try t2.addNode(6);
    const seven = try t2.addNode(7);

    try t2.addEdgeBetween(four, five);
    try t2.addEdgeBetween(four, six);
    try t2.addEdgeBetween(six, seven);

    try t1.addSubtree(three, &t2);

    const children_one = try t1.childrenOf(one);
    defer children_one.deinit();

    try expect(children_one.items.len == 2);
    try expect(children_one.items[0].* == 2);
    try expect(children_one.items[1].* == 3);

    const children_three = try t1.childrenOf(three);
    defer children_three.deinit();

    try expect(children_three.items.len == 1);
    try expect(children_three.items[0].* == 4);

    const children_four = try t1.childrenOf(children_three.items[0]);
    defer children_four.deinit();

    try expect(children_four.items.len == 2);
    try expect(children_four.items[0].* == 5);
    try expect(children_four.items[1].* == 6);

    const children_six = try t1.childrenOf(children_four.items[1]);
    defer children_six.deinit();

    try expect(children_six.items.len == 1);
    try expect(children_six.items[0].* == 7);
}

test "removeNode" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    try t.removeNode(one);
    try expect(try t.isRemoved(one));
}

test "removeSubtree" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);
    try t.addEdgeBetween(three, four);
    try t.addEdgeBetween(three, five);

    const parent, var subtree = try t.removeSubtree(three);
    defer subtree.deinit() catch unreachable;

    try expect(parent == one);

    const root = subtree.getRoot().?;
    try expect(root.* == 3);

    const children_three = try subtree.childrenOf(root);
    defer children_three.deinit();

    try expect(children_three.items.len == 2);
    try expect(children_three.items[0].* == 4);
    try expect(children_three.items[1].* == 5);
}

test "addEdgeBetween" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);

    try t.addEdgeBetween(one, two);
    try expect((try t.parentOf(two)).? == one);

    const children = try t.childrenOf(one);
    defer children.deinit();

    try expect(children.items.len == 1);
    try expect(children.items[0] == two);

    const three = try t.addNode(3);
    try expectError(error.InvalidEdge, t.addEdgeBetween(three, one));
}

test "removeEdgeBetween" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);

    try t.addEdgeBetween(one, two);
    try t.removeEdgeBetween(one, two);
    try expectError(error.InvalidEdge, t.removeEdgeBetween(one, two));
}

test "parentOf" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);

    try t.addEdgeBetween(one, two);

    try expect(try t.parentOf(one) == null);
    try expect((try t.parentOf(two)).? == one);

    try t.removeEdgeBetween(one, two);

    try expect(try t.parentOf(two) == null);
}

test "childrenOf" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);

    const children_one = try t.childrenOf(one);
    defer children_one.deinit();

    const children_two = try t.childrenOf(two);
    defer children_two.deinit();

    try expect(children_one.items.len == 2);
    try expect(children_one.items[0] == two);
    try expect(children_one.items[1] == three);

    try expect(children_two.items.len == 0);

    try t.removeEdgeBetween(one, two);

    const children_one_after = try t.childrenOf(one);
    defer children_one_after.deinit();

    try expect(children_one_after.items.len == 1);
    try expect(children_one_after.items[0] == three);
}

test "shrink" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    try t.addEdgeBetween(one, two);
    try t.shrink();

    // NOTE: All element pointers are invalidated after calling shrink
    try expectError(error.InvalidNode, t.parentOf(one));
    try expectError(error.InvalidNode, t.parentOf(two));
    try expectError(error.InvalidNode, t.parentOf(three));
    try expectError(error.InvalidNode, t.parentOf(four));
    try expectError(error.InvalidNode, t.parentOf(five));

    const root = t.getRoot().?;
    try expect(root.* == 1);

    const root_children = try t.childrenOf(root);
    defer root_children.deinit();

    try expect(root_children.items.len == 1);
    try expect(root_children.items[0].* == 2);

    // TODO: Check for internal memory savings somehow
}

test "getNthNode" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    try expect(try t.getNthNode(0) == one);
    try expect(try t.getNthNode(1) == two);
    try expect(try t.getNthNode(2) == three);
    try expect(try t.getNthNode(3) == four);
    try expect(try t.getNthNode(4) == five);
}

test "format" {
    const PrintableUsize = struct {
        const Self = @This();

        inner: usize,

        pub fn format(
            self: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            try writer.print("{d}", .{self.inner});
        }
    };

    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(PrintableUsize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(.{ .inner = 1 });
    const two = try t.addNode(.{ .inner = 2 });
    const three = try t.addNode(.{ .inner = 3 });
    const four = try t.addNode(.{ .inner = 4 });
    const five = try t.addNode(.{ .inner = 5 });

    try t.addEdgeBetween(one, two);
    try t.addEdgeBetween(one, three);
    try t.addEdgeBetween(three, four);
    try t.addEdgeBetween(three, five);

    const string = try std.fmt.allocPrint(
        allocator,
        "{}",
        .{t},
    );
    defer allocator.free(string);

    try expect(std.mem.eql(u8, string, "(1 2 (3 4 5))"));
}

test "iterator next" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    var it = t.iterator(0);

    try expect(it.next() == one);
    try expect(it.next() == two);
    try expect(it.next() == three);
    try expect(it.next() == four);
    try expect(it.next() == five);
    try expect(it.next() == null);
    try expect(it.next() == null);
}

test "iterator prev" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);

    var it = t.iterator(0);

    try expect(it.next() == one);
    try expect(it.next() == two);
    try expect(it.prev() == two);
    try expect(it.prev() == one);
    try expect(it.prev() == null);
    try expect(it.prev() == null);
    try expect(it.next() == one);
}

test "iterator peek" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);

    var it = t.iterator(0);

    try expect(it.next() == one);
    try expect(it.peek() == two);
    try expect(it.peek() == two);
}

test "iterator set" {
    var gpa = test_init_allocator();
    defer test_deinit_allocator(&gpa);

    const allocator = gpa.allocator();

    var t = Tree(usize).init(allocator);
    defer t.deinit() catch unreachable;

    const one = try t.addNode(1);
    const two = try t.addNode(2);
    const three = try t.addNode(3);
    const four = try t.addNode(4);
    const five = try t.addNode(5);

    var it = t.iterator(0);

    try expect(it.next() == one);
    try expect(it.next() == two);
    try expect(it.next() == three);
    try expect(it.next() == four);
    try expect(it.next() == five);
    try expect(it.next() == null);

    it.set(1);
    try expect(it.next() == two);
}
