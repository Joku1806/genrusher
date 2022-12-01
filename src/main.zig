const std = @import("std");
const board = @import("board.zig");

pub fn main() anyerror!void {
    var b = board.Board.init();
    try b.read_from_file("test.txt");

    const m: board.Move = .{
        .pos = 1,
        .step = 2,
    };

    if (b.is_legal_move(m)) {
        std.debug.print("Move {any} is legal, doing it!\n", .{m});
        b.do_move(m);
        b.undo_move(m);
    }

    std.debug.print("Board after undoing Move: {any}", .{b});
}
