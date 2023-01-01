const std = @import("std");
const ArrayList = std.ArrayList;

const board_m = @import("board.zig");
const Board = board_m.Board;

const RepositoryEntry = struct {
    index: usize,
    board: Board,
};

const BoardRepository = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    entries: ArrayList(RepositoryEntry),
    prng: std.rand.DefaultPrng,

    fn init(allocator: std.mem.Allocator) Self {
        var prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            try std.os.getrandom(std.mem.asBytes(&seed));
            break :blk seed;
        });

        return .{
            .allocator = allocator,
            .entries = std.ArrayList(RepositoryEntry).init(allocator),
            .prng = prng,
        };
    }

    fn read_from(self: *Self, filename: []const u8) !void {
        const file = try std.fs.cwd().openFile(
            filename,
            .{ .read = true },
        );
        defer file.close();

        var i: usize = 0;
        while (true) : (i += 1) {
            const fen = try file.reader().readUntilDelimiterOrEofAlloc(self.allocator, "\n", 1024);
            defer self.allocator.free(fen);

            if (fen) |val| {
                var b = Board.init();
                try b.parse(val);

                self.entries.append(.{
                    .index = i,
                    .board = b,
                });
            } else {
                break;
            }
        }
    }

    fn dump_to(self: *Self, filename: []const u8) !void {
        const file = try std.fs.cwd().createFile(
            filename,
            .{ .read = true, .write = true },
        );
        defer file.close();

        for (self.entries) |entry| {
            const fen = try entry.board.fen(self.allocator);
            defer self.allocator.free(fen);

            try file.write(fen);
            try file.write("\n");
        }
    }

    fn get_random_entry(self: *Self) RepositoryEntry {
        const index = self.prng.random().uintLessThan(usize, self.entries.items.len);
        return self.entries.items[index];
    }

    fn update_entry(self: *Self, entry: RepositoryEntry) void {
        if (entry.index >= self.entries.items.len) return;
        self.entries.replaceRange(entry.index, 1, .{entry});
    }
};
