const std = @import("std");
const board = @import("board.zig");

pub fn main() anyerror!void {
    const text = "6:6:?:?:IBBoooIooLDDJAALooJoKEEMFFKooMGGHHHM";
    var b = board.Board.init();
    try b.parse(text);

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
