#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as
#     `perl Algorithm::Pair::Best.t'

#########################

use strict;
use IO::File;
use Test::More tests => 20;

BEGIN {
    use_ok('Algorithm::Pair::Best')
};

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $pair;

##
## create Pair object.
##
our @scoreSubs = (
#     sub { # difference in rating.
#         my ($my, $candidate) = @_;
# 
#         return -(abs($my->rating - $candidate->rating));
#     },
    sub { # difference in rating.
        my ($my, $candidate, $explain) = @_;

        my $score = -(abs($my->rating - $candidate->rating));
        return sprintf "rating:%5.1f", $score if ($explain);
        return $score;
    },
);

eval { $pair = Algorithm::Pair::Best->new(scoreSubs =>\@scoreSubs); };
is( $@, '',                     'return from new ATourn object'  );
ok( defined $pair,              'created Pair object'  );
ok( $pair->isa('Algorithm::Pair::Best'),
                                '   and it\'s the right class'  );

$main::progress = 0;
package Algorithm::Pair::Best;

sub rating { # add method to access ratings (used in scoreSubs above)
    my $my = shift;

    $my->{info}{rating} = shift if (@_);
    return $my->{info}{rating};
}
sub id { # add method to access id
    my $my = shift;

    $my->{info}{id} = shift if (@_);
    return $my->{info}{id};
}
sub progress {  # add progress method
    my ($my, $item0, $item1) = @_;

    die "\$main::progress not defined" unless(defined($main::progress));
    $main::progress++;    # notice that progress has been made
}

package main;

my $ii = 0;
foreach (3, 4, 2, 2.1, 2.7, 1.7, 6, 5.55) {
    $ii++;
    $pair->add( {id => "item $ii",
                 rating => $_, }
                );
}

my @pairs;
eval { @pairs = $pair->pick(4) };
is( $@, '',                     'return from pick method'  );
is ($main::progress, 4,         'progress was made');
is ($pair->bestScore, -2.35,    'correct score');
is (@pairs, 8,                  'right number of items paired');

my $s = $scoreSubs[0];
$ii = 0;
foreach ((-1, -0.3, -0.6, -0.45,)) {
    is ($pairs[$ii]->$s($pairs[$ii+1]), $_,     'score result');
    $ii += 2;
}
$ii = 0;
foreach (1, 2, 3, 6, 4, 5, 7, 8) {
    is ($pairs[$ii]->id, "item $_",             'pairing result');
    $ii++;
}
