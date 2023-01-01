const std = @import("std");

const board_m = @import("board.zig");
const Board = board_m.Board;

const AST_m = @import("AST.zig");
const AST = AST_m.AST;

const Rule = struct {
    const Self = @This();

    condition: AST,
    expression: AST,

    pub fn init(allocator: std.mem.Allocator, condition_size: usize, expression_size: usize) !Self {
        var ast: Self = .{
            .condition = try AST.init(allocator),
            .expression = try AST.init(allocator),
        };

        try ast.condition.set_random_boolean_expression(condition_size);
        try ast.expression.set_random_arithmetic_expression(expression_size);

        return ast;
    }

    pub fn is_valid(self: Self) bool {
        return self.condition.is_boolean_expression() and self.expression.is_arithmetic_expression();
    }
};

pub const Brain = struct {
    const Self = @This();

    const min_rules = 5;
    const max_rules = 15;
    const min_rule_size = 10;
    const max_rule_size = 20;

    allocator: std.mem.Allocator,
    prng: std.rand.DefaultPrng,
    rules: std.ArrayList(Rule),

    fn create_random_rules(self: *Self) !void {
        const nrules = self.prng.random().intRangeLessThan(usize, min_rules, max_rules + 1);
        var i: usize = 0;

        while (i < nrules) : (i += 1) {
            const condition_size = self.prng.random().intRangeLessThan(usize, min_rule_size, max_rule_size + 1);
            const expression_size = self.prng.random().intRangeLessThan(usize, min_rule_size, max_rule_size + 1);

            const rule = try Rule.init(self.allocator, condition_size, expression_size);
            try self.rules.append(rule);
        }
    }

    pub fn init(allocator: std.mem.Allocator) !Self {
        var prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            try std.os.getrandom(std.mem.asBytes(&seed));
            break :blk seed;
        });

        var brain: Self = .{
            .allocator = allocator,
            .prng = prng,
            .rules = std.ArrayList(Rule).init(allocator),
        };

        try brain.create_random_rules();
        return brain;
    }

    pub fn evaluate_board(self: *Self, board: Board) f32 {
        // We start with 1.0, since we don't want the case of no rule applying in any situation,
        // resulting in the IDA* search getting stuck in an endless loop.
        var result: f32 = 1.0;

        for (self.rules.items) |rule| {
            if (rule.condition.evaluate(board).boolean == true) {
                result += rule.expression.evaluate(board).number;
            }
        }

        return result;
    }
};
