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

    fn generate_moves(self: *Self) PriorityQueue(Move, {}, move_order) {
        var ranked_moves = PriorityQueue(Move, {}, move_order).init(self.allocator);
        var checked = AutoHashMap(u8, void).init(self.allocator);

        // TODO: Write board method returning car positions.
        var i: u8 = 0;
        while (i < self.board.size()) : (i += 1) {
            if (checked.contains(i)) continue;
            if (!self.board.occupied(i)) continue;

            const range = self.board.calculate_move_range(i);
            if (range.min_step == range.max_step) continue;

            var j = range.min_step;
            while (j <= range.max_step) {
                ranked_moves.add(.{ .pos = i, .step = j });
            }

            const o = self.board.car_orientation_at(pos);
            var sz = self.board.car_size_at(pos);

            var pos = i;
            while (sz > 0) : (sz -= 1) {
                checked.put(pos, {});
                pos = self.board.offset_position(pos, 1, o);
            }
        }

        return ranked_moves;
    }

    pub fn find_solution(self: *Self) bool {
        self.bound = self.brain.evaluate_board(self.board);

        while (true) {
            const t = self.search(0);
            if (self.board.reached_goal()) return true;

            self.bound = t;
            while (self.move_history.items.len > 0) {
                self.board.undo_move(self.move_history.pop());
            }
        }
    }

    // TODO: Needs a way to return failure if maximum move
    // capacity without finding a solution is reached.
    fn search(self: *Self, current_cost: f32) f32 {
        const f = current_cost + self.brain.evaluate_board(self.board);
        if (f > self.bound or self.board.reached_goal()) return f;

        const moves = self.generate_moves();
        var it = moves.iterator();
        for (it.next()) |move| {
            self.board.do_move(move);
            self.move_history.append(move);

            const t = self.search(current_cost + move.cost());
            if (self.board.reached_goal()) return f;
            if (t > self.bound) return t;

            self.board.undo_move(move);
            self.move_history.pop();
        }
    }
};
