# Parser for marked data feed

## Usage

There are two major execution modes:

    <file.pcap>    - print messages
    -r <file.pcap> - reorder and print messages

The second one buffers 3 seconds of messages and reorders them by acceding
accept time.

There is also `-c <file.pcap>` mode for testing reordering which doesn't print
messages but ensures that the result is sorted *(note that this execution mode
consumes memory proportionally to input size)*.

## Implementation details

We use lazy byte strings to achieve streaming-like execution and constant memory
footprint. An alternative solution would be to implement explicit streaming
using Conduit or Pipes libraries. This is something I want to explore later.

We parse PCAP file without using an external library. This is because
Network.Pcap looks like a "heavy" external dependency which requires installed C
library. Also Network. Pcap doesn't expose pcap_next_ex API which makes handling
errors and an end-of-file situation problematic. Likely enough Pcap file format
appears to be quite simple.

Choice of Attoparsec for parsing binary data was arbitrary, I just wanted to
parse some binary data with it. There is also Data.Binary.Get parser which
supports incremental parsing and potentially could be faster due to the absence
of backtracking mechanism.

## Reordering algorithms

During reordering, we use only accept-time timestamps and buffer 3 seconds of
messages. Messages which are older than 3 seconds are printed. After the end of
input, we also print buffered messages. Why this works:

1. We have guarantee that difference between accept-time and packet-time will
   never be more than 3 seconds. So packets which are inside of 3 seconds buffer
   will never be older than packets outside of 3 seconds buffer. It can be
   proved by contradiction:

             a         3sec     b      now
         ---------------|-------------->|

   If `b` older that `a` then `accept-time(b)` older than `accept-time(a)` and
   hence `accept-time(b)` is older than 3 seconds and hence `b` is not inside of
   3sec window which is false.

   So we can print sorted messages which are older than 3 seconds and result
   will be sorted.

2. If we buffer more than 3 seconds then that will not compromise algorithms.

3. Accept-time is always older than packet time. And pcap file stores packet in
   the receiving order. Because of that processing packets from pcap files and
   using only accept-time for comparisons without considering packet-time will
   only cause us to buffer more and hence will not compromise algorithm:

                       3sec            now
         ...........+---|-----------|-->|
         printed    '               ^ accept-time now
                    ^ accept-time 3 sec

## Assumptions

We assume that difference between accept-time and packet-time will never be more
than 3 seconds *(but we don't use packet-time directly, we exploit only the fact
than messages are ordered by packet-time)*.

We assume that pcap file contains udp(ipv4)+ethernet packets.

We assume that there will be no packets which have all of these three fields
equal: packets-time, accept-time, issue-code.

## Links

[Feed format description](http://www.tsurucapital.com/en/code-sample.html)
[Pcap file format description (blog post)](http://www.kroosec.com/2012/10/a-look-at-pcap-file-format.html)
