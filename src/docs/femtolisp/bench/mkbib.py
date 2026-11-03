"""A synthetic BibTeX file of 300 references, with repeated authors and years."""
import random, sys

random.seed(7)
fam = ["Knuth", "van der Hoeven", "Gubinelli", "Hofmanov{\\'a}", "Doe", "Roe",
       "Poe", "Smith", "Jones", "M{\\\"u}ller", "Lamport", "Dijkstra", "Turing",
       "Noether", "Hilbert", "Gauss", "Euler", "Riemann", "Weil", "Serre"]
giv = ["Donald E.", "Joris", "Massimiliano", "Martina", "Jane", "Richard",
       "Edgar A.", "Adam", "Mary", "Hans", "Leslie", "Edsger W.", "Alan",
       "Emmy", "David", "Carl F.", "Leonhard", "Bernhard", "Andr{\\'e}",
       "Jean-Pierre"]
words = ("fast evaluation of holonomic functions near regular singularities "
         "complex transseries solutions to algebraic differential equations "
         "the art computer programming quantum field theory stochastic "
         "analysis rough paths large deviations for random matrices").split()
out = []
for i in range(300):
    n = random.choice([1, 1, 2, 2, 3, 5])
    idx = random.sample(range(20), n)
    au = " and ".join("%s, %s" % (fam[j], giv[j]) for j in idx)
    title = " ".join(w.capitalize()
                     for w in random.sample(words, random.randint(4, 9)))
    year = random.choice(range(1990, 2005))
    kind = random.choice(["article", "article", "article", "book",
                          "incollection", "inproceedings", "phdthesis",
                          "techreport"])
    f = ["author={%s}" % au, "title={%s}" % title, "year=%d" % year]
    if kind == "article":
        f += ["journal={Journal of %s}" % random.choice(words).capitalize(),
              "volume=%d" % random.randint(1, 90),
              "number=%d" % random.randint(1, 12),
              "pages={%d--%d}" % (100 + i, 120 + i),
              "doi={10.1000/x.%d}" % i]
    elif kind == "book":
        f += ["publisher={Springer}", "address={Berlin}", "edition={Second}"]
    elif kind in ("incollection", "inproceedings"):
        f += ["booktitle={Proceedings of the %s Conference}"
              % random.choice(words).capitalize(),
              "editor={Smith, Adam and Jones, Mary}", "publisher={EMS Press}",
              "pages={%d--%d}" % (10 + i, 30 + i)]
    elif kind == "phdthesis":
        f += ["school={University of Bonn}"]
    else:
        f += ["institution={INRIA}", "number={%d}" % i]
    out.append("@%s{k%d, %s}" % (kind, i, ", ".join(f)))
open(sys.argv[1], "w").write("\n".join(out) + "\n")
