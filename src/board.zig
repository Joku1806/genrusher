const std = @import("std");

const expect = std.testing.expect;
const expectError = std.testing.expectError;
const expectApproxEqAbs = std.testing.expectApproxEqAbs;

const File = std.fs.File;
const PackedIntArray = std.packed_int_array.PackedIntArray;

const Orientation = enum {
    Horizontal,
    Vertical,
};

const Direction = enum {
    Forward,
    Backward,
};

pub const Move = struct {
    const Self = @This();

    pos: u8,
    step: i8,

    pub fn cost(self: Self) u8 {
        return std.math.absInt(self.step);
    }
};

pub const MoveRange = struct { pos: u8, min_step: i8, max_step: i8 };

const ParseError = error{
    InvalidFormat,
    IllegalBoardDimensions,
    IllegalCarSize,
    DuplicateCar,
    GoalCarMissing,
    MultipleGoalCars,
};

const BoardError = error{
    ExpectedOccupiedField,
    PositionOutOfBounds,
    InvalidMove,
};

const Field = packed struct {
    // Checks if the field is occupied by a car.
    // There is no separate flag needed for the goal car,
    // since it will be the only one in its lane.
    occupied: bool = false,
    // Since two cars in a lane can't swap places,
    // alternating this bit between cars provides a memory
    // efficient way of distinguishing them. This is only possible
    // because the Board uses a separate horizontal and vertical mask.
    pattern: u1 = 0,
};

pub const Board = struct {
    const Self = @This();
    // NOTE: It is very unlikely that we want to solve a board
    // larger than 8x8, so the maximum number of fields is 64.
    const max_fields: comptime_int = 64;

    horizontal_mask: PackedIntArray(Field, max_fields),
    vertical_mask: PackedIntArray(Field, max_fields),
    width: u4,
    height: u4,
    goal_lane: u4,
    goal_orientation: Orientation,
    relative_difficulty: ?f32,
    min_moves: ?usize,

    pub fn init() Self {
        return .{
            .horizontal_mask = PackedIntArray(Field, max_fields).initAllTo(.{ .occupied = false }),
            .vertical_mask = PackedIntArray(Field, max_fields).initAllTo(.{ .occupied = false }),
            .width = undefined,
            .height = undefined,
            .goal_lane = undefined,
            .goal_orientation = undefined,
            .relative_difficulty = undefined,
            .min_moves = undefined,
        };
    }

    fn size(self: *const Self) u8 {
        return @as(u8, self.width) * self.height;
    }

    fn to_position(self: *const Self, row: u4, column: u4) u8 {
        return @as(u8, row) * self.width + column;
    }

    fn extract_row(self: *const Self, pos: u8) u4 {
        return @intCast(u4, pos / self.width);
    }

    fn extract_column(self: *const Self, pos: u8) u4 {
        return @intCast(u4, pos % self.width);
    }

    fn offset_position(self: *const Self, pos: u8, step: i8, o: Orientation) ?u8 {
        const offset: i16 = switch (o) {
            .Vertical => self.width,
            .Horizontal => 1,
        };

        const target = pos + offset * step;
        // takes care of vertical bounds check and any other oob situations
        if (target < 0 or target >= self.size()) return null;

        const clamped = @intCast(u8, target);
        if (o == .Horizontal and self.extract_row(clamped) != self.extract_row(pos)) return null;

        return clamped;
    }

    fn field_occupied(self: *const Self, pos: u8) bool {
        if (pos >= self.size()) return false;
        return self.vertical_mask.get(pos).occupied or self.horizontal_mask.get(pos).occupied;
    }

    fn car_orientation_at(self: *const Self, pos: u8) BoardError!Orientation {
        if (pos >= self.size()) return BoardError.PositionOutOfBounds;
        if (!self.field_occupied(pos)) return BoardError.ExpectedOccupiedField;
        return if (self.vertical_mask.get(pos).occupied) .Vertical else .Horizontal;
    }

    fn car_size_at(self: *const Self, pos: u8) BoardError!usize {
        const o = try self.car_orientation_at(pos);
        var sz: usize = 0;
        const mask = switch (o) {
            .Vertical => &self.vertical_mask,
            .Horizontal => &self.horizontal_mask,
        };

        const pattern = mask.get(pos).pattern;
        var c: ?u8 = pos;
        while (c) |p| : (c = self.offset_position(c.?, 1, o)) {
            if (!mask.get(p).occupied or mask.get(p).pattern != pattern) break;
            sz += 1;
        }

        return sz;
    }

    fn field_character_at(self: *const Self, pos: u8) u8 {
        var o = self.car_orientation_at(pos) catch return 'o';

        if (o == .Vertical and o == self.goal_orientation and self.extract_column(pos) == self.goal_lane or
            o == .Horizontal and o == self.goal_orientation and self.extract_row(pos) == self.goal_lane)
        {
            return 'A';
        }

        switch (o) {
            .Vertical => return if (self.vertical_mask.get(pos).pattern == 0) 'B' else 'C',
            .Horizontal => return if (self.horizontal_mask.get(pos).pattern == 0) 'D' else 'E',
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        var i: u8 = 0;
        while (i < self.size()) : (i += 1) {
            if (i % self.width == 0) {
                try writer.writeAll("\n");
            }

            var ch = self.field_character_at(i);
            try writer.print("{u}", .{ch});
        }

        try writer.print("\nRelative Difficulty: {?}\nMinimum moves to solve: {?}", .{
            self.relative_difficulty, self.min_moves,
        });
    }

    pub fn fen(self: *const Self, allocator: std.mem.Allocator) ![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        const writer = list.writer();

        try writer.print("{}:{}:", .{ self.width, self.height });

        if (self.relative_difficulty) |val| {
            try writer.print("{}:", .{val});
        } else {
            try writer.writeAll("?:");
        }

        if (self.min_moves) |val| {
            try writer.print("{}:", .{val});
        } else {
            try writer.writeAll("?:");
        }

        var i: u8 = 0;
        while (i < self.size()) : (i += 1) {
            var ch = self.field_character_at(i);
            try writer.print("{u}", .{ch});
        }

        return list.toOwnedSlice();
    }

    fn parse_width(self: *Self, text: []const u8) error{InvalidFormat}!usize {
        const wsep = std.mem.indexOf(u8, text, ":") orelse return error.InvalidFormat;
        if (wsep == 0) return error.InvalidFormat;

        self.width = std.fmt.parseInt(u4, text[0..wsep], 10) catch |err| {
            std.debug.print("{}\n", .{err});
            return error.InvalidFormat;
        };

        return wsep + 1;
    }

    fn parse_height(self: *Self, text: []const u8) error{InvalidFormat}!usize {
        const hsep = std.mem.indexOf(u8, text, ":") orelse return error.InvalidFormat;
        if (hsep == 0) return error.InvalidFormat;

        self.height = std.fmt.parseInt(u4, text[0..hsep], 10) catch |err| {
            std.debug.print("{}\n", .{err});
            return error.InvalidFormat;
        };

        return hsep + 1;
    }

    fn parse_difficulty(self: *Self, text: []const u8) error{InvalidFormat}!usize {
        const dsep = std.mem.indexOf(u8, text, ":") orelse return error.InvalidFormat;
        if (dsep == 0) return error.InvalidFormat;

        self.relative_difficulty = switch (text[0]) {
            '?' => null,
            else => std.fmt.parseFloat(f32, text[0..dsep]) catch |err| {
                std.debug.print("{}\n", .{err});
                return error.InvalidFormat;
            },
        };

        return dsep + 1;
    }

    fn parse_min_moves(self: *Self, text: []const u8) error{InvalidFormat}!usize {
        const msep = std.mem.indexOf(u8, text, ":") orelse return error.InvalidFormat;
        if (msep == 0) return error.InvalidFormat;

        self.min_moves = switch (text[0]) {
            '?' => null,
            else => std.fmt.parseInt(usize, text[0..msep], 10) catch |err| {
                std.debug.print("{}\n", .{err});
                return error.InvalidFormat;
            },
        };

        return msep + 1;
    }

    // The format for the board string is as follows:
    // Dimensions: width x height
    // Board difficulty: [0, 1]f32 or "?" if unknown
    // Length of minimal solution: usize or "?" if unknown
    // Board FEN: follows the mapping from https://www.michaelfogleman.com/rush/ (section Database Format)
    // Between every field, ":" is used as a separator.
    //
    // A standard unsolved board would for example be:
    // 6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM
    pub fn parse(self: *Self, text: []const u8) !void {
        var offset = try self.parse_width(text);
        offset += try self.parse_height(text[offset..]);
        offset += try self.parse_difficulty(text[offset..]);
        offset += try self.parse_min_moves(text[offset..]);

        const board_fen = text[offset..];

        if (board_fen.len != self.size() or board_fen.len > max_fields) {
            return ParseError.IllegalBoardDimensions;
        }

        var seen_goal_car = false;
        var horz_pattern: u1 = 0;
        // Save the current vertical pattern for each row, as we do not iterate in vertical order.
        // For our small board sizes of u4, we can just use an array. For larger boards,
        // we probably should just lookup the last vertical pattern in the current column.
        var vert_patterns: [std.math.maxInt(u4)]u1 = undefined;

        for (board_fen) |c, i| {
            if (c == 'o' or c == '.' or self.field_occupied(@intCast(u8, i))) {
                continue;
            }

            const o: Orientation = if (c != board_fen[i + 1] or i + 1 == self.width) .Vertical else .Horizontal;

            const pattern = switch (o) {
                .Horizontal => &horz_pattern,
                .Vertical => &vert_patterns[i % self.width],
            };

            const mask = switch (o) {
                .Horizontal => &self.horizontal_mask,
                .Vertical => &self.vertical_mask,
            };

            const sz = blk: {
                var pos: ?u8 = @intCast(u8, i);
                var j: usize = 0;
                while (pos) |p| : (pos = self.offset_position(pos.?, 1, o)) {
                    if (board_fen[p] != c) break;
                    mask.set(p, .{ .occupied = true, .pattern = pattern.* });
                    j += 1;
                }
                break :blk j;
            };

            if (sz < 2 or sz > 3) {
                return error.IllegalCarSize;
            }

            pattern.* = ~pattern.*;

            if (c == 'A') {
                seen_goal_car = true;
                // NOTE: Goal is always on the right/bottom side of the board
                // (depending on the orientation) and in the lane of the goal car.
                self.goal_orientation = o;
                self.goal_lane = switch (o) {
                    .Vertical => self.extract_column(@intCast(u8, i)),
                    .Horizontal => self.extract_row(@intCast(u8, i)),
                };
            }
        }

        if (!seen_goal_car) {
            return error.GoalCarMissing;
        }

        var goal_cars: usize = 0;
        var pos: ?u8 = switch (self.goal_orientation) {
            .Horizontal => self.to_position(self.goal_lane, 0),
            .Vertical => self.to_position(0, self.goal_lane),
        };

        while (pos) |p| : (pos = self.offset_position(pos.?, 1, self.goal_orientation)) {
            if (!self.field_occupied(p)) continue;
            const co = self.car_orientation_at(p) catch unreachable;
            if (co != self.goal_orientation) continue;

            goal_cars += 1;
            if (goal_cars > 1) return error.MultipleGoalCars;
            const skip = self.car_size_at(p) catch unreachable;
            pos = self.offset_position(p, @intCast(i8, skip - 1), self.goal_orientation);
        }
    }

    pub fn reached_goal(self: *const Self) bool {
        const mask = switch (self.goal_orientation) {
            .Vertical => &self.vertical_mask,
            .Horizontal => &self.horizontal_mask,
        };

        const goal_field = switch (self.goal_orientation) {
            .Vertical => self.to_position(self.height - 1, self.goal_lane),
            .Horizontal => self.to_position(self.goal_lane, self.width - 1),
        };

        return mask.get(goal_field).occupied;
    }

    pub fn is_legal_move(self: *const Self, move: Move) bool {
        if (move.step == 0) return false;
        if (!self.field_occupied(move.pos)) return false;

        const o = self.car_orientation_at(move.pos) catch unreachable;
        const sz = self.car_size_at(move.pos) catch unreachable;

        const sign = std.math.sign(move.step);
        var start = switch (sign) {
            1 => self.offset_position(move.pos, @intCast(i8, sz), o),
            -1 => self.offset_position(move.pos, -1, o),
            else => unreachable,
        } orelse return false;
        const stop = self.offset_position(start, move.step, o) orelse return false;

        var it: ?u8 = start;
        while (it) |f| : (it = self.offset_position(it.?, sign, o)) {
            if (f == stop) return true;
            if (self.field_occupied(f)) return false;
        }

        return false;
    }

    fn step_limit(self: *const Self, pos: u8, dir: Direction) i8 {
        const o = self.car_orientation_at(pos) catch unreachable;
        const sz = self.car_size_at(pos) catch unreachable;

        const sign: i8 = if (dir == Direction.Forward) 1 else -1;
        var cpos = switch (dir) {
            Direction.Forward => self.offset_position(pos, @intCast(i8, sz), o),
            Direction.Backward => self.offset_position(pos, -1, o),
        };

        var step: i8 = 0;
        while (cpos) |p| : (cpos = self.offset_position(cpos.?, sign, o)) {
            if (self.field_occupied(p)) break;
            step += sign;
        }

        return step;
    }

    pub fn calculate_move_range(self: *const Self, pos: u8) MoveRange {
        return .{
            .pos = pos,
            .min_step = self.step_limit(pos, Direction.Backward),
            .max_step = self.step_limit(pos, Direction.Forward),
        };
    }

    // NOTE: Will try doing any move, even if it is illegal!
    // Consequences include crashes and corrupted board states.
    // This function should only be called with moves from generate_moves()
    // or those that were checked by is_legal_move() beforehand.
    // The same of course also applies to undo_move().
    pub fn do_move(self: *Self, move: Move) void {
        const o = self.car_orientation_at(move.pos) catch unreachable;
        // FIXME: Breaks when any other field than top part of car is passed.
        // There should be a function that converts any position into a position
        // representative of the car (top field).
        const sz = self.car_size_at(move.pos) catch unreachable;
        const mask = switch (o) {
            .Vertical => &self.vertical_mask,
            .Horizontal => &self.horizontal_mask,
        };

        var pos = move.pos;
        const target = self.offset_position(move.pos, move.step, o);
        const sign = std.math.sign(move.step);
        while (pos != target) : (pos = self.offset_position(pos, sign, o).?) {
            const source = switch (sign) {
                1 => pos,
                -1 => self.offset_position(pos, @intCast(i8, sz - 1), o),
                else => unreachable,
            };

            const destination = switch (sign) {
                1 => self.offset_position(pos, @intCast(i8, sz), o),
                -1 => self.offset_position(pos, -1, o),
                else => unreachable,
            };

            mask.set(destination.?, mask.get(source.?));
            mask.set(source.?, .{ .occupied = false });
        }
    }

    pub fn undo_move(self: *Self, move: Move) void {
        const o: Orientation = blk: {
            const t = self.offset_position(move.pos, move.step, .Vertical) orelse break :blk .Horizontal;
            const oc = self.car_orientation_at(t) catch break :blk .Horizontal;
            if (oc == .Vertical) break :blk .Vertical else break :blk .Horizontal;
        };

        const reverse: Move = .{
            .pos = self.offset_position(move.pos, move.step, o).?,
            .step = -move.step,
        };

        self.do_move(reverse);
    }

    pub fn car_positions(self: *const Self, allocator: std.mem.Allocator) std.ArrayList(u8) {
        var checked = std.StaticBitSet(max_fields).initEmpty();
        var positions = std.ArrayList(u8).init(allocator);

        var i: u8 = 0;
        while (i < self.size()) : (i += 1) {
            if (checked.isSet(i)) continue;
            if (!self.occupied(i)) continue;
            positions.append(i);

            const o = self.board.car_orientation_at(i);
            var sz = self.board.car_size_at(i);

            var pos = i;
            while (sz > 0) : (sz -= 1) {
                checked.set(pos);
                pos = self.board.offset_position(pos, 1, o);
            }
        }

        return positions;
    }

    // Computes a lower-bound estimate of the number of moves required
    // to move each vehicle out of the goal car’s path. This has to be done
    // recursively for the vehicles blocking these vehicles, and so on. There
    // are also some redundancy checks needed to avoid counting the same
    // vehicle more than once. When having to choose between two possible direction
    // of moving a vehicle, we compute both and retain the minimal value.
    pub fn heuristic_blockers_lower_bound(self: *Self) f32 {
        _ = self;
        return 0.0;
    }

    // Counts each vehicles Manhattan distance from a deduced goal board
    // containing a clear path to the goal, where all vehicles (and vehicles
    // blocking them) have been forcibly positioned in possible locations in
    // which they are no longer blocking the goal car
    // NOTE: How to deduce the goal board? The paper says this part required
    // some "complex reasoning" for doing this, but doesn't elaborate further.
    pub fn heuristic_goal_distance(self: *Self) f32 {
        _ = self;
        return 0.0;
    }

    // Same as GoalDistance, but also adds number of vehicles between each
    // car and its designated location.
    pub fn heuristic_hybrid(self: *Self) f32 {
        _ = self;
        return 0.0;
    }

    // Did the last move taken position the vehicle at a location that no other
    // vehicle can occupy?
    // FIXME: Brain needs the last move as context
    pub fn heuristic_is_move_to_secluded(self: *Self, last_move: Move) bool {
        _ = self;
        _ = last_move;
        return 0.0;
    }

    // Did the last move made add new possible moves?
    pub fn heuristic_is_releasing_move(self: *Self, last_move: Move) bool {
        _ = self;
        _ = last_move;
        return 0.0;
    }

    // FIXME: Brain needs the initial board as context
    pub fn heuristic_initial_board_distance(self: *Self, initial: *Self) f32 {
        _ = self;
        _ = initial;
        return 0.0;
    }

    pub fn heuristic_phase_by_distance(self: *Self, initial: *Self) f32 {
        const initial_distance = self.heuristic_initial_board_distance(initial);
        const goal_distance = self.heuristic_goal_distance();

        return initial_distance / (initial_distance + goal_distance);
    }

    pub fn heuristic_phase_by_blockers(self: *Self, initial: *Self) f32 {
        const initial_distance = self.heuristic_initial_board_distance(initial);
        const blockers = self.heuristic_blockers_lower_bound();

        return initial_distance / (initial_distance + blockers);
    }

    // The number of nodes expanded from the parent of the current node.
    // NOTE: The paper isn't really clear on what this is supposed to mean.
    // I would assume it is supposed to be the number of other moves that
    // were tried before this one, so essentially the ranking of this move
    // by the brain.
    // In this case the brain would somehow need to relay this information
    // to this function, but I don't really know how you would do that.
    pub fn heuristic_number_of_siblings(self: *Self) f32 {
        _ = self;
        return 0.0;
    }

    // Returns the difficulty of solving the puzzle as a number in [0, 1].
    // If a difficulty estimate (using the ratio of solves / total attempts)
    // is known, this will be used. Otherwise, an estimated difficulty will
    // be calculated using the estimated number of moves needed to solve the puzzle.
    // NOTE: The first case can be wildly unreliable, if the number of attempted solves
    // is low. Maybe there should be a single heuristic instead, which combines both cases.
    pub fn heuristic_difficulty(self: *const Self) f32 {
        if (self.relative_difficulty) |diff| return diff;
        // NOTE: I assume a normal distribution here, so the best guess
        // is a perfectly average puzzle ;)
        if (self.min_moves == null) return 0.5;

        // FIXME: Needs to be adjusted to my definition of moves
        // as the number of fields to be crossed, instead of the
        // flat move count, from where this number came from.
        // NOTE: Maybe this should not be done, since the number
        // of crossed fields has no effect on the computational
        // difficulty of the puzzle, because we allow multi-step
        // moves to be made. In this case, maybe the move definition
        // should be changed?
        const hardest_puzzle_solution_len: comptime_int = 51.0;
        return self.min_moves.? / hardest_puzzle_solution_len;
    }

    // Original heuristic, not specified in the paper
    // Returns the number of possible moves relative to the board size.
    pub fn heuristic_free_space(self: *const Self) f32 {
        var positions = self.board.car_positions(self.allocator);
        defer positions.deinit();

        var nmoves: usize = 0;
        for (positions.items) |pos| {
            const range = self.board.calculate_move_range(pos);
            if (range.min_step == range.max_step) continue;
            nmoves += std.math.absInt(range.max_step - range.min_step);
        }

        return nmoves / self.size();
    }
};

test "parsing board info missing" {
    const texts = [_][]const u8{ "", ":", "666:2:", "::4:", "3:2:::AAABBB", "::?:?:" };

    for (texts) |t| {
        var b = Board.init();
        try expectError(ParseError.InvalidFormat, b.parse(t));
    }
}

test "parsing oversized board" {
    const text = "9:9:?:?:ooooooooooooooBooooooooBooDoooAAAooDooooCoooDooooCooooooooooooooooooooooooooooooo";

    var b = Board.init();
    try expectError(ParseError.IllegalBoardDimensions, b.parse(text));
}

test "parsing incomplete board" {
    const text = "4:4:?:?:oCCoDoooDAAoDoo";

    var b = Board.init();
    try expectError(ParseError.IllegalBoardDimensions, b.parse(text));
}

test "parsing goal car missing" {
    const text = "6:6:?:?:oBBBooDDoCCoooRoooEERSTToooSoooFFSoo";

    var b = Board.init();
    try expectError(ParseError.GoalCarMissing, b.parse(text));
}

test "parsing car on goal car lane" {
    const text = "6:6:?:?:oooooooooooooAAoBBoooooooooooooooooo";

    var b = Board.init();
    try expectError(ParseError.MultipleGoalCars, b.parse(text));
}

test "parsing oversized car" {
    const text = "8:2:?:?:AAAAAAooooBBBBBB";

    var b = Board.init();
    try expectError(ParseError.IllegalCarSize, b.parse(text));
}

test "parsing valid board" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";

    var b = Board.init();
    b.parse(text) catch unreachable;
}

test "parsing board with complete info" {
    const text = "6:6:0.5432:12:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";

    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.width == 6);
    try expect(b.height == 6);
    try expect(b.relative_difficulty != null);
    try expect(b.min_moves != null);

    try expectApproxEqAbs(b.relative_difficulty.?, 0.5432, 3 * std.math.floatEps(@TypeOf(b.relative_difficulty.?)));
    try expect(b.min_moves.? == 12);
}

test "position offsets" {
    const text = "4:4:?:?:ooooAAoooooooooo";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.offset_position(0, -1, .Vertical) == null);
    try expect(b.offset_position(0, -20, .Vertical) == null);
    try expect(b.offset_position(0, -1, .Horizontal) == null);
    try expect(b.offset_position(0, -20, .Horizontal) == null);

    try expect(b.offset_position(1, 2, .Vertical).? == 9);
    try expect(b.offset_position(3, 20, .Vertical) == null);
    try expect(b.offset_position(1, 2, .Horizontal).? == 3);
    try expect(b.offset_position(6, 20, .Horizontal) == null);

    try expect(b.offset_position(16, -1, .Vertical).? == 12);
}

test "field occupied" {
    const text = "4:4:?:?:ooBoAABooooooooo";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.field_occupied(2));
    try expect(b.field_occupied(5));
    try expect(!b.field_occupied(14));
    try expect(!b.field_occupied(180));
}

test "car orientation" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.car_orientation_at(0) catch unreachable == .Vertical);
    try expect(b.car_orientation_at(1) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(9) catch unreachable == .Vertical);
    try expect(b.car_orientation_at(10) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(12) catch unreachable == .Vertical);
    try expect(b.car_orientation_at(13) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(20) catch unreachable == .Vertical);
    try expect(b.car_orientation_at(21) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(23) catch unreachable == .Vertical);
    try expect(b.car_orientation_at(24) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(30) catch unreachable == .Horizontal);
    try expect(b.car_orientation_at(32) catch unreachable == .Horizontal);
    try expectError(BoardError.ExpectedOccupiedField, b.car_orientation_at(7));
    try expectError(BoardError.PositionOutOfBounds, b.car_orientation_at(55));
}

test "car size" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.car_size_at(0) catch unreachable == 2);
    try expect(b.car_size_at(1) catch unreachable == 2);
    try expect(b.car_size_at(9) catch unreachable == 2);
    try expect(b.car_size_at(10) catch unreachable == 2);
    try expect(b.car_size_at(12) catch unreachable == 2);
    try expect(b.car_size_at(13) catch unreachable == 2);
    try expect(b.car_size_at(20) catch unreachable == 2);
    try expect(b.car_size_at(21) catch unreachable == 2);
    try expect(b.car_size_at(23) catch unreachable == 3);
    try expect(b.car_size_at(24) catch unreachable == 2);
    try expect(b.car_size_at(30) catch unreachable == 2);
    try expect(b.car_size_at(32) catch unreachable == 3);
    try expectError(BoardError.ExpectedOccupiedField, b.car_size_at(3));
    try expectError(BoardError.PositionOutOfBounds, b.car_size_at(40));
}

test "move car collision" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(!b.is_legal_move(.{ .pos = 23, .step = -2 }));
}

test "move bounds collision" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(!b.is_legal_move(.{ .pos = 1, .step = 5 }));
}

test "move from out of bounds" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(!b.is_legal_move(.{ .pos = 40, .step = -10 }));
}

test "move undoing" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b1 = Board.init();
    var b2 = Board.init();
    b1.parse(text) catch unreachable;
    b2.parse(text) catch unreachable;

    const m1: Move = .{ .pos = 1, .step = 3 };
    const m2: Move = .{ .pos = 9, .step = -1 };
    const m3: Move = .{ .pos = 13, .step = 3 };

    b2.do_move(m1);
    b2.do_move(m2);
    b2.do_move(m3);

    b2.undo_move(m3);
    b2.undo_move(m2);
    b2.undo_move(m1);

    try expect(b1.vertical_mask.len == b2.vertical_mask.len);
    try expect(b1.horizontal_mask.len == b2.horizontal_mask.len);

    var i: usize = 0;
    while (i < b1.vertical_mask.len) : (i += 1) {
        try expect(std.meta.eql(b1.vertical_mask.get(i), b2.vertical_mask.get(i)));
        try expect(std.meta.eql(b1.horizontal_mask.get(i), b2.horizontal_mask.get(i)));
    }
}

test "move generation" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    const range = b.calculate_move_range(1);
    var i: isize = range.min_step;
    while (i < range.max_step) : (i += 1) {
        if (i == 0) continue;
        try expect(b.is_legal_move(.{ .pos = 1, .step = @intCast(i8, i) }));
    }
}

test "goal orientation" {
    const texts = [_][]const u8{ "2:2:?:?:AAoo", "2:2:?:?:ooAA", "2:2:?:?:AoAo", "2:2:?:?:oAoA" };
    const expected_os = [_]Orientation{ .Horizontal, .Horizontal, .Vertical, .Vertical };

    var i: usize = 0;
    while (i < texts.len) : (i += 1) {
        var b = Board.init();
        b.parse(texts[i]) catch unreachable;

        try expect(b.goal_orientation == expected_os[i]);
    }
}

test "reached goal" {
    const text = "3:1:?:?:AAo";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(!b.reached_goal());
    b.do_move(.{ .pos = 0, .step = 1 });
    try expect(b.reached_goal());
}
