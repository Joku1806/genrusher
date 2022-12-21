const std = @import("std");

const board_m = @import("board.zig");
const Board = board_m.Board;
const Move = board_m.Move;
const MoveRange = board_m.MoveRange;

const brain_m = @import("brain.zig");
const Brain = brain_m.Brain;

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const PriorityQueue = std.PriorityQueue;

pub const IDAstar = struct {
    const Self = @This();

    allocator: Allocator,
    board: Board,
    brain: Brain,
    move_history: ArrayList(Move),
    bound: f32,
    nodes_visited: usize,

    fn search_limit(self: *Self) usize {
        // This can be interpreted as meaning:
        // On average, in every 4 moves we allow one move,
        // that does not contribute to the solution.
        const branching_factor = 4.0 / 3.0;
        // It takes 51 moves to solve the longest Rush Hour puzzle
        // (see https://www.michaelfogleman.com/rush/#HardestPuzzles)
        const depth = @intToFloat(f64, self.board.min_moves orelse 51);

        return @floatToInt(usize, pow(f64, branching_factor, depth));
    }

    // FIXME: We wouldnt need the context hack, if we operated on boards in the first place.
    // The move history would then need to store board states as snapshots instead of moves.
    fn move_order(context: *Self, m1: Move, m2: Move) Order {
        context.board.do_move(m1);
        const a = context.brain.evaluate_board(context.board);
        context.board.undo_move(m1);

        context.board.do_move(m2);
        const b = context.brain.evaluate_board(context.board);
        context.board.undo_move(m2);

        return std.math.order(a, b);
    }

    fn generate_moves(self: *Self) PriorityQueue(Move, self, move_order) {
        var ranked_moves = PriorityQueue(Move, self, move_order).init(self.allocator);

        var positions = self.board.car_positions(self.allocator);
        defer positions.deinit();

        for (positions.items) |pos| {
            const range = self.board.calculate_move_range(pos);
            if (range.min_step == range.max_step) continue;

            var i = range.min_step;
            while (i <= range.max_step) {
                ranked_moves.add(.{ .pos = pos, .step = i });
            }
        }

        return ranked_moves;
    }

    pub fn find_solution(self: *Self) bool {
        self.bound = self.brain.evaluate_board(self.board);

        while (true) {
            const t = self.search(0) orelse return false;
            if (self.board.reached_goal()) return true;

            self.nodes_visited = 0;
            self.bound = t;
            while (self.move_history.items.len > 0) {
                self.board.undo_move(self.move_history.pop());
            }
        }
    }

    // TODO: Needs a way to return failure if maximum move
    // capacity without finding a solution is reached.
    fn search(self: *Self, current_cost: f32) ?f32 {
        if (self.nodes_visited > self.search_limit()) return null;

        const f = current_cost + self.brain.evaluate_board(self.board);
        if (f > self.bound or self.board.reached_goal()) return f;

        const moves = self.generate_moves();
        var it = moves.iterator();
        for (it.next()) |move| {
            self.board.do_move(move);
            self.move_history.append(move);
            self.nodes_visited += 1;

            const t = self.search(current_cost + move.cost()) orelse return null;
            if (self.board.reached_goal()) return f;
            if (t > self.bound) return t;

            self.board.undo_move(move);
            self.move_history.pop();
        }
    }
};
