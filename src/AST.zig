const std = @import("std");

const expect = std.testing.expect;

const tree_m = @import("tree.zig");
const Tree = tree_m.Tree;

const board_m = @import("board.zig");
const Board = board_m.Board;

const EvaluationResult = union(enum) {
    boolean: bool,
    number: f32,
};

const Token = union(enum) {
    OpPlus: void,
    OpMinus: void,
    OpLt: void,
    OpEq: void,
    OpLogicalOr: void,
    OpLogicalNot: void,
    HfBlockersLowerBound: void,
    HfGoalDistance: void,
    HfFreeSpace: void,
    HfDifficulty: void,
    HfMoveBound: void,
    PrimitiveNumber: f32,

    pub fn takes_input(self: Token, T: EvaluationResult) bool {
        switch (self) {
            .OpPlus, .OpMinus, .OpLt, .OpEq => return T == .number,
            .OpLogicalNot, .OpLogicalOr => return T == .boolean,
            .HfBlockersLowerBound, .HfGoalDistance, .HfFreeSpace, .HfDifficulty, .HfMoveBound, .PrimitiveNumber => return false,
        }
    }

    pub fn outputs(self: Token) EvaluationResult {
        switch (self) {
            .OpPlus, .OpMinus, .HfBlockersLowerBound, .HfGoalDistance, .HfFreeSpace, .HfDifficulty, .HfMoveBound, .PrimitiveNumber => return .{ .number = undefined },
            .OpLt, .OpEq, .OpLogicalNot, .OpLogicalOr => return .{ .boolean = undefined },
        }
    }

    pub fn arity(self: Token) usize {
        switch (self) {
            .HfBlockersLowerBound, .HfGoalDistance, .HfFreeSpace, .HfDifficulty, .HfMoveBound, .PrimitiveNumber => return 0,
            .OpLogicalNot => return 1,
            .OpPlus, .OpMinus, .OpLt, .OpEq, .OpLogicalOr => return 2,
        }
    }

    pub fn is_terminal(self: Token) bool {
        return self.arity() == 0;
    }

    pub fn is_arithmetic_operator(self: Token) bool {
        switch (self) {
            .OpPlus, .OpMinus => return true,
            else => return false,
        }
    }

    pub fn is_logical_operator(self: Token) bool {
        switch (self) {
            .OpLogicalNot, .OpLogicalOr => return true,
            else => return false,
        }
    }

    pub fn is_comparison_operator(self: Token) bool {
        switch (self) {
            .OpLt, .OpEq => return true,
            else => return false,
        }
    }

    pub fn get_random_token(prng: *std.rand.DefaultPrng) Token {
        const rnd = prng.random().uintLessThan(u8, 12);

        switch (rnd) {
            0 => return .OpPlus,
            1 => return .OpMinus,
            2 => return .OpLt,
            3 => return .OpEq,
            4 => return .OpLogicalOr,
            5 => return .OpLogicalNot,
            6 => return .HfBlockersLowerBound,
            7 => return .HfGoalDistance,
            8 => return .HfFreeSpace,
            9 => return .HfDifficulty,
            10 => return .HfMoveBound,
            11 => return Token{ .PrimitiveNumber = prng.random().float(f32) },
            else => unreachable,
        }
    }

    pub fn get_random_terminal(prng: *std.rand.DefaultPrng) Token {
        const rnd = prng.random().uintLessThan(u8, 6);

        switch (rnd) {
            0 => return .HfBlockersLowerBound,
            1 => return .HfGoalDistance,
            2 => return .HfFreeSpace,
            3 => return .HfDifficulty,
            4 => return .HfMoveBound,
            5 => return Token{ .PrimitiveNumber = prng.random().float(f32) },
            else => unreachable,
        }
    }

    pub fn get_random_arithmetic_operator(prng: *std.rand.DefaultPrng) Token {
        const rnd = prng.random().uintLessThan(u8, 2);

        switch (rnd) {
            0 => return .OpPlus,
            1 => return .OpMinus,
            else => unreachable,
        }
    }

    pub fn get_random_logical_operator(prng: *std.rand.DefaultPrng) Token {
        const rnd = prng.random().uintLessThan(u8, 2);

        switch (rnd) {
            0 => return .OpLogicalNot,
            1 => return .OpLogicalOr,
            else => unreachable,
        }
    }

    pub fn get_random_comparison_operator(prng: *std.rand.DefaultPrng) Token {
        const rnd = prng.random().uintLessThan(u8, 2);

        switch (rnd) {
            0 => return .OpLt,
            1 => return .OpEq,
            else => unreachable,
        }
    }

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .OpPlus => try writer.writeAll("+"),
            .OpMinus => try writer.writeAll("-"),
            .OpLt => try writer.writeAll("<"),
            .OpEq => try writer.writeAll("=="),
            .OpLogicalOr => try writer.writeAll("|"),
            .OpLogicalNot => try writer.writeAll("~"),
            .HfBlockersLowerBound => try writer.writeAll("Hf_BlockersLowerBound"),
            .HfGoalDistance => try writer.writeAll("Hf_GoalDistance"),
            .HfFreeSpace => try writer.writeAll("Hf_FreeSpace"),
            .HfDifficulty => try writer.writeAll("Hf_Difficulty"),
            .HfMoveBound => try writer.writeAll("Hf_MoveBound"),
            .PrimitiveNumber => |val| try writer.print("{}", .{val}),
        }
    }
};

pub const AST = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    prng: std.rand.DefaultPrng,
    program: Tree(Token),
    context_board: Board,

    pub fn init(allocator: std.mem.Allocator) !Self {
        const prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            try std.posix.getrandom(std.mem.asBytes(&seed));
            break :blk seed;
        });

        return .{
            .allocator = allocator,
            .prng = prng,
            .program = Tree(Token).init(allocator),
            .context_board = undefined,
        };
    }

    fn child_count_valid(self: *Self, key: usize) bool {
        const children = self.program.childCountOf(key) catch unreachable;
        const node = self.program.getNode(key) catch unreachable;
        return node.arity() == children;
    }

    fn complete_random_tree(self: *Self, node_bound: usize) !void {
        var nodes_created: usize = 0;
        var terminals_needed: usize = 0;
        var active_keys = std.ArrayList(usize).init(self.allocator);
        defer active_keys.deinit();

        try active_keys.append(self.program.root().?);

        while (nodes_created < node_bound - terminals_needed) {
            const t = Token.get_random_token(&self.prng);
            if (t.is_terminal()) continue;

            var added: ?usize = null;
            for (active_keys.items, 0..) |key, i| {
                const node = self.program.getNode(key) catch unreachable;
                if (node.takes_input(t.outputs())) {
                    const a = self.program.addNode(t);
                    try self.program.addEdgeBetween(key, a);
                    // TODO: Check if this causes problems inside the loop.
                    // Although we break right after, so maybe not.
                    if (self.child_count_valid(key)) {
                        _ = active_keys.orderedRemove(i);
                    }
                    added = a;
                    break;
                }
            }

            if (added == null) {
                continue;
            }

            nodes_created += 1;
            try active_keys.append(added.?);

            // NOTE: There is no guarantee that nodes_created + terminals_needed <= node_bound.
            // Because of this, node_bound is a lower bound that may be crossed by a
            // small number of nodes.
            terminals_needed = 0;
            for (active_keys.items) |key| {
                const node = self.program.getNode(key) catch unreachable;
                const children = self.program.childCountOf(key) catch unreachable;
                terminals_needed += node.arity() - children;
            }
        }

        for (active_keys.items) |key| {
            // NOTE: Right now, there is no terminal that outputs a boolean. That means
            // that logical operators are not allowed to be leaves after the end of the first while loop.
            const node = self.program.getNode(key) catch unreachable;
            if (node.is_logical_operator()) {
                while (!self.child_count_valid(key)) {
                    const e1 = self.program.addNode(Token.get_random_comparison_operator(&self.prng));
                    const t1 = self.program.addNode(Token.get_random_terminal(&self.prng));
                    const t2 = self.program.addNode(Token.get_random_terminal(&self.prng));

                    try self.program.addEdgeBetween(key, e1);
                    try self.program.addEdgeBetween(e1, t1);
                    try self.program.addEdgeBetween(e1, t2);
                }
            } else {
                while (!self.child_count_valid(key)) {
                    const t = self.program.addNode(Token.get_random_terminal(&self.prng));
                    try self.program.addEdgeBetween(key, t);
                }
            }
        }
    }

    pub fn set_random_boolean_expression(self: *Self, node_count: usize) !void {
        const start = blk: while (true) {
            const t = Token.get_random_token(&self.prng);
            if (t.is_logical_operator() or t.is_comparison_operator()) {
                break :blk t;
            }
        };

        const added = self.program.addNode(start);
        try self.program.setRoot(added);
        try self.complete_random_tree(node_count - 1);
    }

    pub fn set_random_arithmetic_expression(self: *Self, node_count: usize) !void {
        const start = Token.get_random_arithmetic_operator(&self.prng);
        const added = self.program.addNode(start);
        try self.program.setRoot(added);
        try self.complete_random_tree(node_count - 1);
    }

    fn be_helper(self: *Self, node: *Token) bool {
        if (!child_count_valid(node)) return false;
        if (node.parent) |p| {
            if (!p.value.takes_input(node.value.outputs())) return false;
        }

        switch (node.value) {
            .HfBlockersLowerBound, .HfGoalDistance, .HfFreeSpace, .HfDifficulty, .HfMoveBound, .PrimitiveNumber => return !node.is_root() and node.is_leaf(),
            .OpLogicalNot => return self.be_helper(node.nth_child(0)),
            .OpLogicalOr => return self.be_helper(node.nth_child(0)) and self.be_helper(node.nth_child(1)),
            .OpLt, .OpEq => return self.ae_helper(node.nth_child(0)) and self.ae_helper(node.nth_child(1)),
            .OpPlus, .OpMinus => return !node.is_root() and self.ae_helper(node.nth_child(0)) and self.ae_helper(node.nth_child(1)),
        }
    }

    fn ae_helper(self: *Self, node: *Token) bool {
        if (!child_count_valid(node)) return false;
        if (node.parent) |p| {
            if (!p.value.takes_input(node.value.outputs())) return false;
        }

        switch (node.value) {
            .HfBlockersLowerBound, .HfGoalDistance, .HfFreeSpace, .HfDifficulty, .HfMoveBound, .PrimitiveNumber => return true,
            .OpLogicalNot, .OpLt, .OpEq, .OpLogicalOr => return false,
            .OpPlus, .OpMinus => return self.ae_helper(node.nth_child(0)) and self.ae_helper(node.nth_child(1)),
        }
    }

    pub fn is_syntactically_valid(self: *Self) bool {
        return self.is_boolean_expression() or self.is_arithmetic_expression();
    }

    pub fn is_boolean_expression(self: *Self) bool {
        return self.be_helper(&self.program.root);
    }

    pub fn is_arithmetic_expression(self: *Self) bool {
        return self.ae_helper(&self.program.root);
    }

    pub fn normalize(self: *Self) void {
        _ = self;
    } // TODO

    fn eval_helper(self: *Self, node: *Token) EvaluationResult {
        var buffer = try self.allocator.alloc(EvaluationResult, node.branch_factor());
        defer self.allocator.free(buffer);

        var i: usize = 0;
        const factor = node.branch_factor();
        while (i < factor) : (i += 1) {
            buffer[i] = self.eval_helper(node.nth_child(i));
        }

        switch (node.value) {
            .OpPlus => return .{ .number = buffer[0].number + buffer[1].number },
            .OpMinus => return .{ .number = buffer[0].number - buffer[1].number },
            .OpLt => return .{ .boolean = buffer[0].number < buffer[1].number },
            // FIXME: Should probably use approxEq because floats
            .OpEq => return .{ .boolean = buffer[0].number == buffer[1].number },
            .OpLogicalOr => return .{ .boolean = buffer[0].boolean or buffer[1].boolean },
            .OpLogicalNot => return .{ .boolean = !buffer[0].boolean },
            .HfBlockersLowerBound => return .{ .number = self.context_board.heuristic_blockers_lower_bound() },
            .HfGoalDistance => return .{ .number = self.context_board.heuristic_goal_distance() },
            .HfFreeSpace => return .{ .number = self.context_board.heuristic_free_space() },
            .HfDifficulty => return .{ .number = self.context_board.heuristic_difficulty() },
            .HfMoveBound => return .{ .number = self.context_board.heuristic_move_bound() },
            .PrimitiveNumber => |value| return .{ .number = value },
            else => unreachable,
        }
    }

    pub fn evaluate(self: *Self, board: *Board) EvaluationResult {
        self.context_board = board;
        const res = self.eval_helper(self.program.root);
        self.context_board = undefined;

        return res;
    }
};

test "token typechecking" {
    const plus = Token{ .OpPlus = {} };
    const minus = Token{ .OpMinus = {} };
    const lor = Token{ .OpLogicalOr = {} };
    const lnot = Token{ .OpLogicalNot = {} };
    const lt = Token{ .OpLt = {} };
    const eq = Token{ .OpEq = {} };
    const hf = Token{ .HfMoveBound = {} };
    const number = Token{ .PrimitiveNumber = undefined };

    // Arithmetic Operator + Arithmetic Operand
    try expect(plus.takes_input(number.outputs()));
    // Arithmetic Operator + Boolean Operand
    try expect(!minus.takes_input(lor.outputs()));
    // Equality Operator + Arithmetic Operand
    try expect(lt.takes_input(minus.outputs()));
    // Equality Operator + Boolean Operand
    try expect(!eq.takes_input(lnot.outputs()));
    // Boolean Operator + Boolean Operand
    try expect(lor.takes_input(lnot.outputs()));
    // Boolean Operator + Arithmetic Operand
    try expect(!lnot.takes_input(hf.outputs()));
    // Terminal + anything
    try expect(!number.takes_input(hf.outputs()));
}
