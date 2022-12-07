const std = @import("std");

const expect = std.testing.expect;
const expectError = std.testing.expectError;

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

pub const Move = struct { pos: u8, step: i8 };
pub const MoveRange = struct { pos: u8, min_step: i8, max_step: i8 };

const ParseError = error{
    InvalidFormat,
    IllegalBoardDimensions,
    IllegalCarSize,
    DuplicateCar,
    GoalCarMissing,
    MultipleGoalCars,
};

const Field = packed struct {
    occupied: bool = false, // Checks if the field is occupied by a car. There is no separate flag needed for the goal car, since it will be the only one in its lane and the orientation can be deduced by the goal position.
    pattern: u1 = 0, // Since two cars on a lane can't swap places, alternating this bit between cars provides a memory efficient way of distinguishing them. This is only possible because the Board uses a separate horizontal and vertical mask.
};

pub const Board = struct {
    const Self = @This();

    // TODO: check viability of ArrayBitSet
    // NOTE: It is very unlikely that we want to solve a board larger than 8x8, so we use an array with static size of 64.
    horizontal_mask: PackedIntArray(Field, 64),
    vertical_mask: PackedIntArray(Field, 64),
    width: u4,
    height: u4,
    goal_position: u8,
    relative_difficulty: ?f32,
    min_moves: ?usize,

    pub fn init() Board {
        return .{
            .horizontal_mask = PackedIntArray(Field, 64).initAllTo(.{ .occupied = false }),
            .vertical_mask = PackedIntArray(Field, 64).initAllTo(.{ .occupied = false }),
            .width = 0,
            .height = 0,
            .goal_position = 0,
            .relative_difficulty = null,
            .min_moves = null,
        };
    }

    fn size(self: *Self) u8 {
        return @as(u8, self.width) * self.height;
    }

    fn to_position(self: *Self, row: u4, column: u4) u8 {
        return @as(u8, row) * self.width + column;
    }

    fn extract_row(self: *Self, pos: u8) u4 {
        return @intCast(u4, pos / self.width);
    }

    fn extract_column(self: *Self, pos: u8) u4 {
        return @intCast(u4, pos % self.width);
    }

    fn offset_position(self: *Self, pos: u8, step: i8, o: Orientation) ?u8 {
        const offset: i16 = switch (o) {
            Orientation.Vertical => self.width,
            Orientation.Horizontal => 1,
        };

        if (pos + offset * step < 0) return null;
        const target = @intCast(u8, pos + offset * step);

        switch (o) {
            Orientation.Vertical => if (self.extract_row(target) >= self.height) return null,
            Orientation.Horizontal => if (self.extract_row(target) != self.extract_row(pos)) return null,
        }

        return target;
    }

    fn field_occupied(self: *Self, pos: u8) bool {
        return self.vertical_mask.get(pos).occupied or self.horizontal_mask.get(pos).occupied;
    }

    fn car_orientation_at(self: *Self, pos: u8) Orientation {
        return if (self.vertical_mask.get(pos).occupied) Orientation.Vertical else Orientation.Horizontal;
    }

    fn car_size_at(self: *Self, pos: u8) usize {
        const o = self.car_orientation_at(pos);
        var sz: usize = 0;
        const mask = switch (o) {
            Orientation.Vertical => &self.vertical_mask,
            Orientation.Horizontal => &self.horizontal_mask,
        };

        const pattern = mask.get(pos).pattern;
        var c: ?u8 = pos;
        while (c) |p| : (c = self.offset_position(c.?, 1, o)) {
            if (!mask.get(p).occupied or mask.get(p).pattern != pattern) break;
            sz += 1;
        }

        return sz;
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

        if (board_fen.len != self.size() or board_fen.len > 64) {
            return ParseError.IllegalBoardDimensions;
        }

        std.debug.print("Got board {s} with dimensions {}x{}, relative difficulty: {?}, length of minimal solution: {?}\n", .{ board_fen, self.width, self.height, self.relative_difficulty, self.min_moves });

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

            const o = if (c != board_fen[i + 1] or i + 1 == self.width) Orientation.Vertical else Orientation.Horizontal;

            const pattern = switch (o) {
                Orientation.Horizontal => &horz_pattern,
                Orientation.Vertical => &vert_patterns[i % self.width],
            };

            const mask = switch (o) {
                Orientation.Horizontal => &self.horizontal_mask,
                Orientation.Vertical => &self.vertical_mask,
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

            std.debug.print("Parsed {} car '{c}' at position (x: {}, y: {}) with size {}\n", .{ o, c, self.extract_column(@intCast(u8, i)), self.extract_row(@intCast(u8, i)), sz });

            if (c == 'A') {
                seen_goal_car = true;
                // NOTE: Goal is always on the right/bottom side of the board
                // (depending on the orientation) and in the lane of the goal car.
                self.goal_position = switch (o) {
                    Orientation.Horizontal => self.to_position(self.extract_row(@intCast(u8, i)), self.width),
                    Orientation.Vertical => self.to_position(self.height, self.extract_column(@intCast(u8, i))),
                };
            }
        }

        if (!seen_goal_car) {
            return error.GoalCarMissing;
        }

        var goal_cars: usize = 0;
        const go = self.goal_orientation();
        var pos: ?u8 = switch (go) {
            Orientation.Horizontal => self.to_position(self.extract_row(self.goal_position) - 1, 0),
            Orientation.Vertical => self.to_position(0, self.extract_column(self.goal_position)),
        };

        while (pos) |p| : (pos = self.offset_position(pos.?, 1, go)) {
            if (self.field_occupied(p) and self.car_orientation_at(p) == go) {
                goal_cars += 1;
                if (goal_cars > 1) return error.MultipleGoalCars;
                pos = self.offset_position(p, @intCast(i8, self.car_size_at(p) - 1), go);
            }
        }

        std.debug.print("Resulting Board: {any} with size {}\n", .{ self, std.fmt.fmtIntSizeBin(@sizeOf(Board)) });
    }

    // FIXME: returns Horizontal for leftmost Vertical goal.
    fn goal_orientation(self: *Board) Orientation {
        return if (self.goal_position % self.width == 0) Orientation.Horizontal else Orientation.Vertical;
    }

    pub fn reached_goal(self: *Board) bool {
        const o = self.goal_orientation();

        const mask = switch (o) {
            Orientation.Vertical => &self.vertical_mask,
            Orientation.Horizontal => &self.horizontal_mask,
        };

        return mask.get(self.offset_position(self.goal_position, -1, o)).occupied;
    }

    pub fn is_legal_move(self: *Self, move: Move) bool {
        if (move.step == 0) return false;
        if (!self.field_occupied(move.pos)) return false;

        const o = self.car_orientation_at(move.pos);
        const sz = self.car_size_at(move.pos);

        const sign = std.math.sign(move.step);
        var pos = switch (sign) {
            1 => self.offset_position(move.pos, @intCast(i8, sz), o),
            -1 => self.offset_position(move.pos, -1, o),
            else => unreachable,
        };
        const target = self.offset_position(move.pos, move.step, o);
        if (target == null) return false;

        while (pos) |p| : (pos = self.offset_position(pos.?, sign, o)) {
            if (self.field_occupied(p)) return false;
            if (p == target.?) return true;
        }

        return false;
    }

    pub fn step_limit(self: *Self, pos: u8, dir: Direction) i8 {
        const o = self.car_orientation_at(pos);
        const sz = self.car_size_at(pos);

        const sign = if (dir == Direction.Forward) 1 else -1;
        var cpos = switch (dir) {
            Direction.Forward => self.offset_position(pos, @intCast(i8, sz), o),
            Direction.Backward => self.offset_position(pos, -1, o),
            else => unreachable,
        };

        var step: i8 = 0;
        while (cpos) |p| : (cpos = self.offset_position(cpos.?, sign, o)) {
            if (self.field_occupied(p)) break;
            step += sign;
        }

        return step;
    }

    pub fn calculate_move_range(self: *Self, pos: u8) MoveRange {
        return .{
            .pos = pos,
            .min_step = self.step_limit(pos, Direction.Backward),
            .max_step = self.step_limit(pos, Direction.Forward),
        };
    }

    pub fn do_move(self: *Self, move: Move) void {
        const o = self.car_orientation_at(move.pos);
        const sz = self.car_size_at(move.pos);
        const mask = switch (o) {
            Orientation.Vertical => &self.vertical_mask,
            Orientation.Horizontal => &self.horizontal_mask,
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

    pub fn undo_move(self: *Board, move: Move) void {
        const t = self.offset_position(move.pos, move.step, Orientation.Vertical);
        const o = if (self.car_orientation_at(t.?) == Orientation.Vertical) Orientation.Vertical else Orientation.Horizontal;

        const reverse: Move = .{
            .pos = self.offset_position(move.pos, move.step, o).?,
            .step = -move.step,
        };

        self.do_move(reverse);
    }

    // fn heuristic_blockers_lower_bound(self: *Board) f32 {}
    // fn heuristic_goal_distance(self: *Board) f32 {}
    // fn heuristic_free_space(self: *Board) f32 {}
    // fn heuristic_initial_board_distance(self: *Board, initial: *Board) f32 {}
    // fn heuristic_difficulty(self: *Self) f32 {}
    // fn heuristic_move_bound(self: *Self) f32 {}
};

test "offset_position" {
    const text = "4:4:?:?:ooooAAoooooooooo";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.offset_position(0, -1, Orientation.Vertical) == null);
    try expect(b.offset_position(0, -20, Orientation.Vertical) == null);
    try expect(b.offset_position(0, -1, Orientation.Horizontal) == null);
    try expect(b.offset_position(0, -20, Orientation.Horizontal) == null);

    try expect(b.offset_position(1, 2, Orientation.Vertical).? == 9);
    try expect(b.offset_position(3, 20, Orientation.Vertical) == null);
    try expect(b.offset_position(1, 2, Orientation.Horizontal).? == 3);
    try expect(b.offset_position(6, 20, Orientation.Horizontal) == null);

    try expect(b.offset_position(16, -1, Orientation.Vertical).? == 12);
}

test "parsing oversized board" {
    const text = "9:9:?:?:ooooooooooooooBooooooooBooDoooAAAooDooooCoooDooooCooooooooooooooooooooooooooooooo";

    var b = Board.init();
    b.parse(text) catch |err| {
        try expect(err == ParseError.IllegalBoardDimensions);
        return;
    };
    unreachable;
}

test "parsing incomplete board" {
    const text = "4:4:?:?:oCCoDoooDAAoDoo";

    var b = Board.init();
    b.parse(text) catch |err| {
        try expect(err == ParseError.IllegalBoardDimensions);
        return;
    };
    unreachable;
}

test "parsing goal car missing" {
    const text = "6:6:?:?:oBBBooDDoCCoooRoooEERSTToooSoooFFSoo";

    var b = Board.init();
    b.parse(text) catch |err| {
        try expect(err == ParseError.GoalCarMissing);
        return;
    };
    unreachable;
}

test "parsing car on goal car lane" {
    const text = "6:6:?:?:oooooooooooooAAoBBoooooooooooooooooo";

    var b = Board.init();
    b.parse(text) catch |err| {
        try expect(err == ParseError.MultipleGoalCars);
        return;
    };
    unreachable;
}

test "parsing oversized car" {
    const text = "8:2:?:?:AAAAAAooooBBBBBB";

    var b = Board.init();
    b.parse(text) catch |err| {
        try expect(err == ParseError.IllegalCarSize);
        return;
    };
    unreachable;
}

test "parsing valid board" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";

    var b = Board.init();
    b.parse(text) catch unreachable;
}

test "moving from empty field" {}
test "moving from middle of car" {}
test "moving into other car" {}
test "moving from out of bounds" {}
test "moving out of bounds" {}
test "move valid" {}

test "reached goal" {}

test "generate moves" {}
