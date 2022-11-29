const std = @import("std");

const File = std.fs.File;
const PackedIntArray = std.packed_int_array.PackedIntArray;

const Orientation = enum {
    Horizontal,
    Vertical,
};

const Move = struct { pos: u8, step: i8 };

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

    pub fn read_from_file(self: *Board, path: []const u8) (File.OpenError || std.mem.Allocator.Error || ParseError || error{BrokenPipe} ||
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

        // TODO: Fixed Size Buffer statt Allocator benutzen
        // FIXME: Vulnerable to bad input + ugly
        const board_string = try file.readToEndAlloc(allocator, 128);

        const flattened_size = std.mem.replacementSize(u8, board_string, "\n", "");
        const flattened = try allocator.alloc(u8, flattened_size);
        const lines = std.mem.replace(u8, board_string, "\n", "", flattened);

        self.height = @intCast(u4, lines);
        self.width = @intCast(u4, @divExact(flattened.len, self.height));

        if (self.height > std.math.maxInt(u4) or self.width > std.math.maxInt(u4) or @as(u8, self.width) * self.height > self.horizontal_mask.len) {
            return error.IllegalBoardDimensions;
        }

        std.debug.print("Got board {s} with dimensions {}x{}\n", .{ flattened, self.width, self.height });

        var seen_goal_car = false;
        var horz_pattern: u1 = 0;
        // Save the current vertical pattern for each row, as we do not iterate in vertical order.
        var vert_patterns = try allocator.alloc(u1, self.width);

        // follows the mapping from https://www.michaelfogleman.com/rush/ (section Database Format)
        for (flattened) |c, i| {
            if (c == 'o' or c == '.') continue;

            const o = if (c != flattened[i + 1] or i + 1 == self.width) Orientation.Vertical else Orientation.Horizontal;

            switch (o) {
                Orientation.Horizontal => horz_pattern = ~horz_pattern,
                Orientation.Vertical => vert_patterns[i % self.width] = ~vert_patterns[i % self.width],
            }

            const size = switch (o) {
                Orientation.Horizontal => blk: {
                    const lim = self.width - i % self.width;
                    var j: u8 = 0;
                    while (j < lim and flattened[i + j] == c) {
                        self.horizontal_mask.set(i + j, .{
                            .occupied = true,
                            .pattern = horz_pattern,
                        });
                        flattened[i + j] = 'o';
                        j += 1;
                    }
                    break :blk j;
                },
                Orientation.Vertical => blk: {
                    const lim = self.height - i / self.width;
                    var j: u8 = 0;
                    while (j < lim and flattened[i + j * self.width] == c) {
                        self.vertical_mask.set(i + j * self.width, .{
                            .occupied = true,
                            .pattern = vert_patterns[i % self.width],
                        });
                        flattened[i + j * self.width] = 'o';
                        j += 1;
                    }
                    break :blk j;
                },
            };

            if (size < 2 or size > 3) {
                return error.IllegalCarSize;
            }

            std.debug.print("Parsed {} car '{c}' at position (x: {}, y: {}) with size {}\n", .{ o, c, i % self.width, i / self.width, size });

            if (c == 'A') {
                seen_goal_car = true;
                // NOTE: Goal is always on the right/bottom side (depending on the orientation) and in the lane of the goal car
                self.goal_position = switch (o) {
                    Orientation.Horizontal => @intCast(u8, i + self.width - (i % self.width)),
                    Orientation.Vertical => @intCast(u8, (self.height - 1) * self.width + i % self.width),
                };
            }
        }

        if (!seen_goal_car) {
            return error.GoalCarMissing;
        }

        std.debug.print("Resulting Board: {any} with size {}\n", .{ self, std.fmt.fmtIntSizeBin(@sizeOf(Board)) });
    }

    // fn do_move(self: *Board, move: Move) error{IllegalMove}!void {}
    // fn undo_move(self: *Board, move: Move) error{IllegalMove}!void {}

    // fn reached_goal(self: *Board) bool {}

    // fn heuristic_blockers_lower_bound(self: *Board) f32 {}
    // fn heuristic_goal_distance(self: *Board) f32 {}
    // fn heuristic_free_space(self: *Board) f32 {}
    // fn heuristic_initial_board_distance(self: *Board, initial: *Board) f32 {}
};
