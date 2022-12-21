const std = @import("std");
const board = @import("board.zig");
const brain = @import("brain.zig");

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const PriorityQueue = std.PriorityQueue;

pub const IDAstar = struct {
    const Self = @This();

    allocator: Allocator,
    // TODO: find way to fix this ugly hack
    _board: board.Board,
    _brain: brain.Brain,
    move_history: ArrayList(board.Move),
    bound: f32,

    // FIXME: We wouldnt need the context hack, if we operated on boards in the first place.
    // The move history would then need to store board states as snapshots instead of moves.
    fn move_order(context: *Self, m1: Move, m2: Move) Order {
        context._board.do_move(m1);
        const a = self._brain.evaluate_board(context._board);
        context._board.undo_move(m1);

        context._board.do_move(m2);
        const b = self._brain.evaluate_board(context._board);
        context._board.undo_move(m2);

        return std.math.order(a, b);
    }

    fn generate_moves(self: *Self) PriorityQueue(board.Move, {}, move_order) {
        var ranked_moves = PriorityQueue(board.Move, {}, move_order).init(self.allocator);
        var checked = AutoHashMap(u8, void).init(self.allocator);

        // TODO: Write board method returning car positions.
        var i: u8 = 0;
        while (i < self._board.size()) : (i += 1) {
            if (checked.contains(i)) continue;
            if (!self._board.occupied(i)) continue;

            const range = self._board.calculate_move_range(i);
            if (range.min_step == range.max_step) continue;

            var j = range.min_step;
            while (j <= range.max_step) {
                ranked_moves.add(.{ .pos = i, .step = j });
            }

            const o = self._board.car_orientation_at(pos);
            var sz = self._board.car_size_at(pos);

            var pos = i;
            while (sz > 0) : (sz -= 1) {
                checked.put(pos, {});
                pos = self._board.offset_position(pos, 1, o);
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
                self._board.undo_move(self.move_history.pop());
            }
        }
    }

    // TODO: Needs a way to return failure if maximum move
    // capacity without finding a solution is reached.
    fn search(self: *Self, current_cost: f32) f32 {
        const f = current_cost + self._brain.evaluate_board(self._board);
        if (f > self.bound or self._board.reached_goal()) return f;

        const moves = self.generate_moves();
        var it = moves.iterator();
        for (it.next()) |move| {
            self._board.do_move(move);
            self.move_history.append(move);

            const t = self.search(current_cost + move.cost());
            if (self._board.reached_goal()) return f;
            if (t > self.bound) return t;

            self._board.undo_move(move);
            self.move_history.pop();
        }
    }
};
