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
    InvalidField,
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
        return @intCast(pos / self.width);
    }

    fn extract_column(self: *const Self, pos: u8) u4 {
        return @intCast(pos % self.width);
    }

    fn offset_position(self: *const Self, pos: u8, step: i8, o: Orientation) ?u8 {
        const offset: i16 = switch (o) {
            .Vertical => self.width,
            .Horizontal => 1,
        };

        const target = pos + offset * step;
        // takes care of vertical bounds check and any other oob situations
        if (target < 0 or target >= self.size()) return null;

        const clamped: u8 = @intCast(target);
        if (o == .Horizontal and self.extract_row(clamped) != self.extract_row(pos)) return null;

        return clamped;
    }

    fn field_occupied(self: *const Self, pos: u8) bool {
        if (pos >= self.size()) return false;
        return self.vertical_mask.get(pos).occupied or self.horizontal_mask.get(pos).occupied;
    }

    fn field_distance(self: *const Self, f1: u8, f2: u8) !u4 {
        if (self.extract_row(f1) == self.extract_row(f2)) {
            const c1 = self.extract_column(f1);
            const c2 = self.extract_column(f2);
            return if (c1 > c2) c1 - c2 else c2 - c1;
        }

        if (self.extract_column(f1) == self.extract_column(f2)) {
            const r1 = self.extract_row(f1);
            const r2 = self.extract_row(f2);
            return if (r1 > r2) r1 - r2 else r2 - r1;
        }

        return error.InvalidField;
    }

    fn car_orientation_at(self: *const Self, pos: u8) BoardError!Orientation {
        if (pos >= self.size()) return BoardError.PositionOutOfBounds;
        if (!self.field_occupied(pos)) return BoardError.ExpectedOccupiedField;
        return if (self.vertical_mask.get(pos).occupied) .Vertical else .Horizontal;
    }

    // NOTE: could just return 0 in case of errors.
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

    fn car_representative_field(self: *const Self, pos: u8) u8 {
        const o = self.car_orientation_at(pos) catch unreachable;

        var cursor: ?u8 = pos;
        var cursor_adv: ?u8 = self.offset_position(pos, -1, o);

        while (cursor_adv) |c| {
            if (!self.field_occupied(c) or self.field_character_at(c) != self.field_character_at(cursor.?)) break;
            cursor = cursor_adv;
            cursor_adv = self.offset_position(cursor_adv.?, -1, o);
        }

        return cursor.?;
    }

    fn field_character_at(self: *const Self, pos: u8) u8 {
        const o = self.car_orientation_at(pos) catch return 'o';

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

            const ch = self.field_character_at(i);
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
            const ch = self.field_character_at(i);
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

        for (board_fen, 0..) |c, i| {
            if (c == 'o' or c == '.' or self.field_occupied(@intCast(i))) {
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
                var pos: ?u8 = @intCast(i);
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
                    .Vertical => self.extract_column(@intCast(i)),
                    .Horizontal => self.extract_row(@intCast(i)),
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
            pos = self.offset_position(p, @intCast(skip - 1), self.goal_orientation);
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
        const start = switch (sign) {
            1 => self.offset_position(move.pos, @intCast(sz), o),
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
            Direction.Forward => self.offset_position(pos, @intCast(sz), o),
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
                -1 => self.offset_position(pos, @intCast(sz - 1), o),
                else => unreachable,
            };

            const destination = switch (sign) {
                1 => self.offset_position(pos, @intCast(sz), o),
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

    fn intersect_ray(self: *const Self, initial_position: u8, direction: i8, orientation: Orientation) ?u8 {
        var cpos = self.offset_position(initial_position, direction, orientation);
        while (cpos) |p| : (cpos = self.offset_position(cpos.?, direction, orientation)) {
            if (self.field_occupied(p)) return p;
        }

        return null;
    }

    fn intersect_ray_ignoring_orientation(self: *const Self, initial_position: u8, direction: i8, orientation: Orientation, ignored: Orientation) ?u8 {
        var cpos = self.offset_position(initial_position, direction, orientation);
        while (cpos) |p| : (cpos = self.offset_position(cpos.?, direction, orientation)) {
            if (self.field_occupied(p) and self.car_orientation_at(p) catch unreachable != ignored) return p;
        }

        return null;
    }

    fn lane_difference(current: *const Self, initial: *const Self, index: u4, o: Orientation) usize {
        var diff: usize = 0;
        var initial_needle: ?u8 = switch (o) {
            .Vertical => initial.to_position(0, index),
            .Horizontal => initial.to_position(index, 0),
        };
        var current_needle: ?u8 = switch (o) {
            .Vertical => current.to_position(0, index),
            .Horizontal => current.to_position(index, 0),
        };
        const opposite: Orientation = if (o == .Vertical) .Horizontal else .Vertical;

        while (initial_needle != null and current_needle != null) {
            initial_needle = initial.intersect_ray_ignoring_orientation(initial_needle.?, 1, o, opposite);
            current_needle = current.intersect_ray_ignoring_orientation(current_needle.?, 1, o, opposite);

            if (initial_needle != null and current_needle != null) {
                const ival = switch (o) {
                    .Vertical => initial.extract_row(initial_needle.?),
                    .Horizontal => initial.extract_column(initial_needle.?),
                };

                const cval = switch (o) {
                    .Vertical => current.extract_row(current_needle.?),
                    .Horizontal => current.extract_column(current_needle.?),
                };

                diff += @abs(@as(i8, cval) - @as(i8, ival));

                const isz: i8 = @intCast(initial.car_size_at(initial_needle.?) catch unreachable);
                const csz: i8 = @intCast(current.car_size_at(current_needle.?) catch unreachable);
                initial_needle = initial.offset_position(initial_needle.?, isz - 1, o);
                current_needle = current.offset_position(current_needle.?, csz - 1, o);
            }
        }

        return diff;
    }

    fn distance_to(self: *Self, other: *Self) usize {
        var diff: usize = 0;

        var col: u4 = 0;
        while (col < self.width) : (col += 1) {
            diff += self.lane_difference(other, col, .Vertical);
        }

        var row: u4 = 0;
        while (row < self.height) : (row += 1) {
            diff += self.lane_difference(other, row, .Horizontal);
        }

        return diff;
    }

    fn calculate_move_lower_bound(self: *const Self, move: Move, cache: *std.AutoHashMap(Move, f32)) f32 {
        // NOTE: Check if it is really ok to return the bound for a specific
        // move more than once. The description in the paper could be interpreted
        // as meaning that moves should be completely ignored after the initial
        // calculation, i.e. using a HashSet to prevent cycles and returning 0 here.
        if (cache.get(move)) |bound| {
            return bound;
        }

        const o = self.car_orientation_at(move.pos) catch unreachable;
        const sz: i8 = @intCast(self.car_size_at(move.pos) catch unreachable);

        // NOTE: If the required move is impossible, we return Infinity to
        // discard this branch.
        if (self.offset_position(move.pos, move.step, o) == null) {
            return std.math.inf(f32);
        }

        cache.put(move, 0.0) catch unreachable;

        var bound: f32 = 0.0;
        const sign = std.math.sign(move.step);
        var current_pos = switch (sign) {
            -1 => self.offset_position(move.pos, -1, o),
            1 => self.offset_position(move.pos, sz, o),
            else => unreachable,
        };
        const target_pos = self.offset_position(move.pos, move.step, o);

        while (current_pos) |p| : (current_pos = self.offset_position(current_pos.?, sign, o)) {
            if (!self.field_occupied(p) and p != target_pos) continue;

            if (self.field_occupied(p)) {
                const blocker_pos = self.car_representative_field(p);
                const blocker_size: i8 = @intCast(self.car_size_at(blocker_pos) catch unreachable);
                const blocker_orientation = self.car_orientation_at(blocker_pos) catch unreachable;

                if (blocker_orientation == o) {
                    const m: Move = .{ .pos = p, .step = move.step };
                    bound += self.calculate_move_lower_bound(m, cache);
                    break;
                } else {
                    const dist = self.field_distance(blocker_pos, p) catch unreachable;
                    const fstep: i8 = dist + 1;
                    const bstep: i8 = -(blocker_size - fstep + 1);

                    const m1: Move = .{ .pos = blocker_pos, .step = fstep };
                    const m2: Move = .{ .pos = blocker_pos, .step = bstep };
                    const c1 = self.calculate_move_lower_bound(m1, cache);
                    const c2 = self.calculate_move_lower_bound(m2, cache);

                    bound += @min(c1, c2);
                }
            }

            if (bound == std.math.inf(f32)) break;
            if (p == target_pos) break;
        }

        bound += 1.0;
        cache.put(move, bound) catch unreachable;
        return bound;
    }

    // Computes a lower-bound estimate of the number of moves required
    // to move each vehicle out of the goal car’s path. This has to be done
    // recursively for the vehicles blocking these vehicles, and so on. There
    // are also some redundancy checks needed to avoid counting the same
    // vehicle more than once. When having to choose between two possible direction
    // of moving a vehicle, we compute both and retain the minimal value.
    pub fn heuristic_blockers_lower_bound(self: *Self, allocator: std.mem.Allocator) f32 {
        var cache = std.AutoHashMap(Move, f32).init(allocator);
        defer cache.deinit();

        const lpos = switch (self.goal_orientation) {
            .Vertical => self.to_position(0, self.goal_lane),
            .Horizontal => self.to_position(self.goal_lane, 0),
        };

        const opposite: Orientation = if (self.goal_orientation == .Vertical) .Horizontal else .Vertical;
        const goal_car_pos = self.intersect_ray_ignoring_orientation(lpos, 1, self.goal_orientation, opposite).?;
        const goal_car_size = self.car_size_at(goal_car_pos) catch unreachable;
        const goal_pos = switch (self.goal_orientation) {
            .Vertical => self.offset_position(lpos, @intCast(self.height - goal_car_size), self.goal_orientation).?,
            .Horizontal => self.offset_position(lpos, @intCast(self.width - goal_car_size), self.goal_orientation).?,
        };

        const goal_move = .{
            .pos = goal_car_pos,
            .step = self.field_distance(goal_car_pos, goal_pos) catch unreachable,
        };

        return self.calculate_move_lower_bound(goal_move, &cache);
    }

    // Did the last move taken position the vehicle at a location that no other
    // vehicle can occupy?
    // FIXME: Brain needs the previous move as context
    // NOTE: Is the board this function is called on the current or the previous board?
    // For now I assume the current board.
    pub fn heuristic_is_move_to_secluded(self: *Self, last_move: Move) bool {
        self.undo_move(last_move);
        defer self.do_move(last_move);

        const co = self.car_orientation_at(last_move.pos) catch unreachable;
        const target = self.offset_position(last_move.pos, last_move.step, co).?;

        const directions = [2]i8{ -1, 1 };
        const orientations = [2]Orientation{ .Vertical, .Horizontal };

        for (directions) |dir| {
            for (orientations) |o| {
                if (self.intersect_ray(target, dir, o)) |p| {
                    if (self.field_character_at(p) == self.field_character_at(last_move.pos)) continue;
                    const po = self.car_orientation_at(p) catch unreachable;
                    if (po == o) return false;
                }
            }
        }

        return true;
    }

    // Did the last move made add new possible moves?
    pub fn heuristic_is_releasing_move(self: *Self, last_move: Move) bool {
        self.undo_move(last_move);
        defer self.do_move(last_move);

        const co = self.car_orientation_at(last_move.pos) catch unreachable;
        const opposite: Orientation = if (co == .Horizontal) .Vertical else .Horizontal;

        var pos: ?u8 = last_move.pos;
        const target = self.offset_position(last_move.pos, last_move.step, co).?;

        const sign = std.math.sign(last_move.step);
        const same_dir_check = switch (sign) {
            1 => last_move.pos,
            -1 => self.offset_position(last_move.pos, last_move.step + 1, co).?,
            else => unreachable,
        };

        if (self.intersect_ray(same_dir_check, -sign, co)) |intersected| {
            const io = self.car_orientation_at(intersected) catch unreachable;
            if (io == co) return true;
        }

        while (pos) |p| : (pos = self.offset_position(pos.?, sign, co)) {
            if (p == target) break;

            if (self.intersect_ray(p, 1, opposite)) |intersected| {
                const io = self.car_orientation_at(intersected) catch unreachable;
                if (io == opposite) return true;
            }

            if (self.intersect_ray(p, -1, opposite)) |intersected| {
                const io = self.car_orientation_at(intersected) catch unreachable;
                if (io == opposite) return true;
            }
        }

        return false;
    }

    // FIXME: Brain needs the initial board as context
    pub fn heuristic_initial_board_distance(self: *Self, initial: *Self) f32 {
        return @floatFromInt(self.distance_to(initial));
    }

    // Counts each vehicles Manhattan distance from a deduced goal board
    // containing a clear path to the goal, where all vehicles (and vehicles
    // blocking them) have been forcibly positioned in possible locations in
    // which they are no longer blocking the goal car
    // NOTE: How to deduce the goal board? The paper says this part required
    // some "complex reasoning" for doing this, but doesn't elaborate further.
    pub fn heuristic_goal_board_distance(self: *Self, goal: *Self) f32 {
        return @floatFromInt(self.distance_to(goal));
    }

    // Same as GoalDistance, but also adds number of vehicles between each
    // car and its designated location.
    pub fn heuristic_hybrid(self: *Self, goal: *Self) f32 {
        _ = self;
        _ = goal;
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
    // In this case this function is not needed, the heuristic can be implemented
    // in the brain itself.
    pub fn heuristic_number_of_siblings(self: *Self) f32 {
        _ = self;
        return 0.0;
    }

    // Returns the difficulty of solving the puzzle as a number in [0, 1],
    // using an average of the dynamic difficulty estimate
    // (ratio of fails / total attempts) and relative length of the
    // solution to other puzzles. We use an average, because the first estimate
    // itself can be wildly unreliable if the number of total attempts is low.
    pub fn heuristic_difficulty(self: *const Self) f32 {
        const longest_solution_len: comptime_int = 51.0;
        const dynamic_estimate: f32 = self.relative_difficulty orelse 0.5;
        const length_estimate: f32 = if (self.min_moves) |n| n / longest_solution_len else 0.5;

        return (dynamic_estimate + length_estimate) / 2.0;
    }

    // Original heuristic, not specified in the paper
    // Returns the number of possible moves relative to the board size.
    pub fn heuristic_free_space(self: *const Self, allocator: std.mem.Allocator) f32 {
        var positions = self.car_positions(allocator);
        defer positions.deinit();

        var nmoves: usize = 0;
        for (positions.items) |pos| {
            const range = self.calculate_move_range(pos);
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

test "car representative field" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.car_representative_field(23) == 23);
    try expect(b.car_representative_field(29) == 23);
    try expect(b.car_representative_field(35) == 23);

    try expect(b.car_representative_field(32) == 32);
    try expect(b.car_representative_field(33) == 32);
    try expect(b.car_representative_field(34) == 32);
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
        try expect(b.is_legal_move(.{ .pos = 1, .step = @intCast(i) }));
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

test "heuristic is move to secluded" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    const secluded = .{ .pos = 23, .step = -1 };
    b.do_move(secluded);
    try expect(b.heuristic_is_move_to_secluded(secluded));
    b.undo_move(secluded);

    const not_secluded = .{ .pos = 9, .step = -1 };
    b.do_move(not_secluded);
    try expect(!b.heuristic_is_move_to_secluded(not_secluded));
    b.undo_move(not_secluded);
}

test "heuristic is releasing move" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    const releasing = .{ .pos = 9, .step = -1 };
    b.do_move(releasing);
    try expect(b.heuristic_is_releasing_move(releasing));
    b.undo_move(releasing);

    const not_releasing = .{ .pos = 1, .step = 2 };
    b.do_move(not_releasing);
    try expect(!b.heuristic_is_releasing_move(not_releasing));
    b.undo_move(not_releasing);
}

test "heuristic initial board distance" {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var initial = Board.init();
    initial.parse(text) catch unreachable;

    var b = Board.init();
    b.parse(text) catch unreachable;

    b.do_move(.{ .pos = 1, .step = 3 });
    b.do_move(.{ .pos = 23, .step = -1 });
    b.do_move(.{ .pos = 32, .step = 1 });
    b.do_move(.{ .pos = 20, .step = 1 });

    try expect(b.heuristic_initial_board_distance(&initial) == 6.0);
}

test "heuristic blockers lower bound" {
    const allocator = std.testing.allocator;
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = Board.init();
    b.parse(text) catch unreachable;

    try expect(b.heuristic_blockers_lower_bound(allocator) == 2.0);
}
