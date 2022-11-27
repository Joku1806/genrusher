const std = @import("std");
const ArrayList = std.ArrayList;

const Vec2D = struct { x: isize, y: isize };
const Move = struct { pos: Vec2D, dir: Vec2D };
const Car = struct { pos: Vec2D, size: Vec2D, goal_car: bool };

const Board = struct {
    // NOTE: beschränkt Spielfeld auf 64 Felder
    horizontal_mask: u64,
    vertical_mask: u64,
    cars: ArrayList(Car),
    corner_ul: Vec2D,
    corner_lr: Vec2D,
    goal_position: Vec2D,

    fn read_from_file(path: []const u8) error{ PathNotFound, AccessDenied, ParseError }!void {}

    fn do_move(self: *Board, move: Move) error{IllegalMove}!void {}
    fn undo_move(self: *Board, move: Move) error{IllegalMove}!void {}

    fn reached_goal(self: *Board) bool {}

    fn heuristic_blockers_lower_bound(self: *Board) f32 {}
    fn heuristic_goal_distance(self: *Board) f32 {}
    fn heuristic_free_space(self: *Board) f32 {}
    fn heuristic_initial_board_distance(self: *Board, initial: *Board) f32 {}
};
