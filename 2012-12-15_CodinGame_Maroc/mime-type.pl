use 5.010;
use strict;
use warnings;

my $n = <>;
my $q = <>;
my %mime;
for (1..$n) {
    my ($ext,$mime) = split /\s+/, <>;
    $mime{lc $ext} = $mime;
}
for (1..$q) {
    my $file = <>;
    my ($ext) = $file =~ /\.(\w+)$/;
    say $mime{lc $ext} // "UNKNOWN";
}
