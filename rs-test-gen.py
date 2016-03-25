#!/usr/bin/python
import sys
import time

def simple_send(argv):
    txt = argv.pop(0)
    sys.stdout.write(txt)
    return

def count_lines(argv):
    txt = argv[0]
    n1 = int(argv[1])
    n2 = int(argv[2])
    n = n1
    while n <= n2:
        line = "%s%d\n" % (txt, n)
        sys.stdout.write(line)
        n += 1
    return
    
def seq_send(argv):
    while argv:
        txt = argv.pop(0)
        sys.stdout.write(txt)
        sys.stdout.flush()
        time.sleep(0.1)
    return

def main():
    sys.argv.pop(0)
    if len(sys.argv) == 0:
        return
    cmd = sys.argv.pop(0)
    tbl = {
        "simple_send" : simple_send,
        "count_lines" : count_lines,
        "seq_send"    : seq_send,
    }
    if cmd not in tbl:
        sys.exit(0)
    func = tbl[cmd]
    func(sys.argv)
    return

if __name__ == "__main__":
    main()
