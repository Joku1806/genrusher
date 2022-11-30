const std = @import("std");

const File = std.fs.File;
const PackedIntArray = std.packed_int_array.PackedIntArray;

const Orientation = enum {
    Horizontal,
    Vertical,
};

pub const Move = struct { pos: u8, step: i8 };

const ParseError = error{
    IllegalBoardDimensions,
    IllegalCarSize,
    DuplicateCar,
    GoalCarMissing,
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

    pub fn init() Board {
        return .{
            .horizontal_mask = PackedIntArray(Field, 64).initAllTo(.{ .occupied = false }),
            .vertical_mask = PackedIntArray(Field, 64).initAllTo(.{ .occupied = false }),
            .width = 0,
            .height = 0,
            .goal_position = 0,
        };
    }

    fn determine_board_dimensions(self: *Self, contents: []u8) error{IllegalBoardDimensions}!void {
        const lim: usize = std.math.maxInt(u4);
        const height: usize = std.mem.count(u8, contents, "\n");

        if (height > lim) return error.IllegalBoardDimensions;
        if ((contents.len - height) % height != 0) return error.IllegalBoardDimensions;
        const width: usize = (contents.len - height) / height; // subtract height because of newlines

        if (width * height > 64) return error.IllegalBoardDimensions;

        self.width = @intCast(u4, width);
        self.height = @intCast(u4, height);
    }

    pub fn read_from_file(self: *Self, path: []const u8) (File.OpenError || std.mem.Allocator.Error || ParseError || error{BrokenPipe} ||
        error{ConnectionResetByPeer} ||
        error{ConnectionTimedOut} ||
        error{InputOutput} ||
        error{NotOpenForReading} ||
        error{OperationAborted})!void {
        const file = try std.fs.cwd().openFile(
            path,
            .{},
        );
        defer file.close();

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        const text = try file.readToEndAlloc(allocator, 128);
        try self.determine_board_dimensions(text);

        const replaced = std.mem.replace(u8, text, "\n", "", text);

        std.debug.print("Got board {s} with dimensions {}x{}\n", .{ text, self.width, self.height });

        var seen_goal_car = false;
        var horz_pattern: u1 = 0;
        // Save the current vertical pattern for each row, as we do not iterate in vertical order.
        var vert_patterns = try allocator.alloc(u1, self.width);

        // follows the mapping from https://www.michaelfogleman.com/rush/ (section Database Format)
        for (text[0 .. text.len - replaced]) |c, i| {
            if (c == 'o' or c == '.') continue;

            const o = if (c != text[i + 1] or i + 1 == self.width) Orientation.Vertical else Orientation.Horizontal;

            switch (o) {
                Orientation.Horizontal => horz_pattern = ~horz_pattern,
                Orientation.Vertical => vert_patterns[i % self.width] = ~vert_patterns[i % self.width],
            }

            const sz = switch (o) {
                Orientation.Horizontal => blk: {
                    const lim = self.width - i % self.width;
                    var j: u8 = 0;
                    while (j < lim and text[i + j] == c) {
                        self.horizontal_mask.set(i + j, .{
                            .occupied = true,
                            .pattern = horz_pattern,
                        });
                        text[i + j] = 'o';
                        j += 1;
                    }
                    break :blk j;
                },
                Orientation.Vertical => blk: {
                    const lim = self.height - i / self.width;
                    var j: u8 = 0;
                    while (j < lim and text[i + j * self.width] == c) {
                        self.vertical_mask.set(i + j * self.width, .{
                            .occupied = true,
                            .pattern = vert_patterns[i % self.width],
                        });
                        text[i + j * self.width] = 'o';
                        j += 1;
                    }
                    break :blk j;
                },
            };

            if (sz < 2 or sz > 3) {
                return error.IllegalCarSize;
            }

            std.debug.print("Parsed {} car '{c}' at position (x: {}, y: {}) with size {}\n", .{ o, c, i % self.width, i / self.width, sz });

            if (c == 'A') {
                seen_goal_car = true;
                // NOTE: Goal is always on the right/bottom side of the board (depending on the orientation) and in the lane of the goal car
                self.goal_position = switch (o) {
                    Orientation.Horizontal => @intCast(u8, i + self.width - (i % self.width) + 1),
                    Orientation.Vertical => @intCast(u8, self.height * self.width + i % self.width),
                };
            }
        }

        if (!seen_goal_car) {
            return error.GoalCarMissing;
        }

        std.debug.print("Resulting Board: {any} with size {}\n", .{ self, std.fmt.fmtIntSizeBin(@sizeOf(Board)) });
    }

    fn size(self: *Self) u8 {
        return @as(u8, self.width) * self.height;
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
        const offset = switch (o) {
            Orientation.Vertical => self.width,
            Orientation.Horizontal => 1,
        };

        var c = pos;
        const pattern = mask.get(pos).pattern;
        while (mask.get(c).pattern == pattern) {
            sz += 1;
            c += offset;
        }

        return sz;
    }

    fn offset_position(self: *Self, pos: u8, step: i8, o: Orientation) ?u8 {
        if (pos >= self.size()) return null;

        const offset: i16 = switch (o) {
            Orientation.Vertical => self.width,
            Orientation.Horizontal => 1,
        };

        if (pos + offset * step < 0 or pos + offset * step >= self.size()) return null;
        return @intCast(u8, pos + offset * step);
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

    // fn reached_goal(self: *Board) bool {}

    // fn heuristic_blockers_lower_bound(self: *Board) f32 {}
    // fn heuristic_goal_distance(self: *Board) f32 {}
    // fn heuristic_free_space(self: *Board) f32 {}
    // fn heuristic_initial_board_distance(self: *Board, initial: *Board) f32 {}
};
