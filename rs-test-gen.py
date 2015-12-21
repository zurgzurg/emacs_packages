import sys

def simple_send(argv):
    txt = argv.pop(0)
    print txt
    return

def main():
    sys.argv.pop(0)
    cmd = sys.argv.pop(0)
    tbl = {
        "simple_send" : simple_send
    }
    if cmd not in tbl:
        sys.exit(0)
    func = tbl[cmd]
    func(sys.argv)
    return

if __name__ == "__main__":
    main()
