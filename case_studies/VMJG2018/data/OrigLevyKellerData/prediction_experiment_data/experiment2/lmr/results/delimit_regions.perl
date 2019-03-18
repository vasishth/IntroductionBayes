#!/usr/bin/perl -w

# adds region delimiters based on items.

# BEWARE: this works only if locale is properly set, and the correct
# setting is OS-dependent.  The setting used here seems to work under
# Linux, but not under Cygwin.

use locale;
use POSIX 'locale_h';
setlocale(LC_CTYPE, "de_DE.iso88591");
#setlocale(LC_CTYPE, "de");

$delim = "^";

while (<>) {

    $line = $_;

    # Condition 1
    if ($line =~ m/^(trial C1I\d+D0 inline .*)( \w+ \w+)( \w+,)( \w+)( \w+.\w+,)(.*)( \w+ \w+)( \w+ \w+,)(.*)( \w+\.)$/) {
	print (join $delim, ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10));
	print "\n";
    }
    # Condition 2
    elsif ($line =~ m/^(trial C2I\d+D0 inline .*)( \w+ \w+)( \w+,)( \w+)( \w+.\w+,)(.*)( \w+ \w+)( \w+ \w+,)(.*)( \w+\.)$/) {
	print (join $delim, ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10));
	print "\n";

    }
    # Condition 3
    elsif ($line =~ m/^(trial C3I\d+D0 inline .*)( \w+ \w+)( \w+,)( \w+)( \w+.\w+,)(.*)( \w+ \w+)( \w+ \w+,)(.*)( \w+\.)$/) {
	print (join $delim, ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10));
	print "\n";

    }
    # Condition 4
    elsif ($line =~ m/^(trial C4I\d+D0 inline .*)( \w+ \w+)( \w+,)( \w+)( \w+.\w+,)(.*)( \w+ \w+)( \w+ \w+,)(.*)( \w+\.)$/) {
	print (join $delim, ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10));
	print "\n";
    }
    else {
	print "## no match: $line";
    }

}
