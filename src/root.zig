const std = @import("std");
const posix = std.posix;

pub const HostPort = struct {
    host: []const u8,
    port: u16,
};

pub fn checkHostConnections(allocator: std.mem.Allocator, hosts_ports: []const [2][]const u8, timeoutMs: u32) ![][]u8 {
    var successful = std.ArrayList([]u8).init(allocator);

    for (hosts_ports) |host_port| {
        const host_str = host_port[0];
        const port_str = host_port[1];

        if (port_str.len == 0) continue; // Skip empty port strings
        // var port = std.fmt.parseInt(u16, port_str, 10) catch continue; // Ignore invalid ports

        var hints = posix.addrinfo{
            .ai_family = posix.AF_UNSPEC,
            .ai_socktype = posix.SOCK_STREAM,
        };

        var addrs: *posix.addrinfo = null;
        const gai_err = posix.getaddrinfo(host_str.ptr, port_str.ptr, &hints, &addrs);
        if (gai_err != 0) {
            continue; // Skip host that failed to resolve
        }
        defer posix.freeaddrinfo(addrs);

        var connected: bool = false;
        for (addrs) |addr| {
            if (addrs == null) {
                break;
            }
            const sockfd = posix.socket(addr.ai_family, posix.SOCK_STREAM, addr.ai_protocol);
            if (sockfd == -1) continue; // Skip on socket creation failure

            // Make the socket non-blocking
            const flags = posix.fcntl(sockfd, posix.F_GETFL, 0);
            if (flags != -1) {
                _ = posix.fcntl(sockfd, posix.F_SETFL, flags | posix.O_NONBLOCK);
            }

            _ = posix.connect(sockfd, addr.ai_addr, addr.ai_addrlen);

            var ret: i32 = 0;
            ret = posix.getErrno(); // Capture the current error state
            if (posix.errno(ret) == posix.EINPROGRESS) {
                var pfd = posix.pollfd{
                    .fd = sockfd,
                    .events = posix.POLLOUT,
                };

                var timeout_spec: posix.timespec = .{
                    .tv_sec = timeoutMs / 1000,
                    .tv_nsec = ((timeoutMs % 1000) * 1000000),
                };

                const poll_ret = posix.ppoll(&pfd, 1, &timeout_spec, null);
                if (poll_ret == -1) {
                    _ = posix.close(sockfd);
                    continue;
                }

                if (poll_ret == 0) { // Timed out
                    _ = posix.close(sockfd);
                    continue;
                }

                var optval: i32 = undefined;
                const optlen: usize = @sizeOf(i32);
                if (posix.getsockopt(sockfd, posix.SOL_SOCKET, posix.SO_ERROR, &optval, optlen) == 0 and optval == 0) {
                    connected = true;
                }

                _ = posix.close(sockfd);
            } else if (ret != -1) { // Success
                connected = true;
                _ = posix.close(sockfd);
            } else { // Other error occurred
                _ = posix.close(sockfd);
                continue;
            }
        }

        if (connected) {
            const host_copy = try allocator.dupe(u8, host_str);
            successful.append(host_copy) catch continue;
        }
    }

    return successful.toOwnedSlice();
}
