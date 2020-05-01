import time

from bs4 import BeautifulSoup

def measure(runs, name, f):
    generator = xrange(runs)

    start_time = time.time()

    for i in generator:
        f()

    duration = time.time() - start_time
    average = duration / runs * 1000000

    print "%s: %.0f us" % (name, average)

def main():
    html = open("test/pages/google.html").read()

    def parse():
        return BeautifulSoup(html)

    measure(1000, "parse", parse)
    soup = parse()

    def traverse():
        count = 0
        for node in soup.descendants:
            count += 1
        return count

    assert traverse() > 100
    measure(1000, "traverse", traverse)

    def select():
        return soup.select("form[action*=search]")[0]

    assert select().name == "form"
    measure(1000, "select", select)

    def select_all():
        count = 0
        for node in soup.select("*"):
            count += 1
        return count

    assert select_all() > 10
    measure(1000, "select_all", select_all)

if __name__ == "__main__":
    main ()
