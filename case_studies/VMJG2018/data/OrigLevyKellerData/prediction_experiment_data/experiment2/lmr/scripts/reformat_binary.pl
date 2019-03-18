#!/usr/bin/perl -w

print "subj\titem\tdat\tadj\tregion1\tregion2\tregion3\tregion4\tregion5\tregion6\tregion7\tregion8\tregion9\tregion10\n";

while (<>) {

    # remove empty cells
    s/(^\s+\d\d?\s+\d\d?\s+)0(\s+.*)/$1NULL$2/;

    # recoded 0 and 100 as 0 and 1
    s/100/  1/g;

    # recode conditions
    s/(^\s+\d\d?\s+\d\d?\s+)1(\s+.*)/$1main    main$2/;
    s/(^\s+\d\d?\s+\d\d?\s+)2(\s+.*)/$1main    sub $2/;
    s/(^\s+\d\d?\s+\d\d?\s+)3(\s+.*)/$1sub     main$2/;
    s/(^\s+\d\d?\s+\d\d?\s+)4(\s+.*)/$1sub     sub $2/;

    if (!(/NULL/)) {print};

}
