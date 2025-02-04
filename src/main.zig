const std = @import("std");
const argsParser = @import("args");
const waitforsocket = @import("root.zig");

pub fn main() !u8 {
    const allocator = std.heap.page_allocator;

    const Options = struct {
        absTimeout: ?u32 = null,
        required: u16 = 0,
        timeout: u64 = 5000,
        faildelay: u64 = 1000,
        help: bool = false,

        pub const shorthands = .{
            .h = "help",
        };

        pub const meta = .{
            .option_docs = .{
                .absTimeout = "absolute timeout (default: no timeout)",
                .required = "how many connections required (default all)",
                .timeout = "connect/retry timeout (ms) (default: 5000)",
                .faildelay = "seconds to delay before retrying (default: 1)",
                .help = "help help",
            },
        };
    };

    const options = argsParser.parseForCurrentProcess(Options, allocator, .print) catch return 1;
    defer options.deinit();

    std.debug.print("executable name: {?s} targets...\n", .{options.executable_name});

    std.debug.print("parsed options:\n", .{});
    inline for (std.meta.fields(@TypeOf(options.options))) |fld| {
        std.debug.print("\t{s} = {any}\n", .{
            fld.name,
            @field(options.options, fld.name),
        });
    }

    std.debug.print("parsed positionals:\n", .{});
    for (options.positionals) |arg| {
        std.debug.print("\t'{s}'\n", .{arg});
    }

    if (options.options.help) {
        try argsParser.printHelp(Options, options.executable_name orelse "demo", std.io.getStdOut().writer());
    }

    const pairs = &[_][2][]const u8{
        .{ "example.com", "80" },
        .{ "google.com", "443" },
    };

    // Timeout of 1500ms (1.5 seconds)
    const timeout_ms = 1500;

    const results = try waitforsocket.checkHostConnections(allocator, pairs, timeout_ms);
    for (results, 0..) |success, i| {
        std.debug.print("Connection to {s}:{d} => {s}\n", .{ pairs[i].host, pairs[i].port, if (success) "success" else "failed" });
    }

    return 0;
}
