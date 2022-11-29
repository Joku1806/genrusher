const std = @import("std");
const board = @import("board.zig");

pub fn main() anyerror!void {
    var b = board.Board.init();
    try b.read_from_file("test.txt");
}
