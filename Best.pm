# $Id: Best.pm,v 1.10 2005/01/24 02:21:46 reid Exp $

#   Algorithm::Pair::Best.pm
#
#   Copyright (C) 2004, 2005 Reid Augustin reid@HelloSix.com
#
#   This library is free software; you can redistribute it and/or modify it
#   under the same terms as Perl itself, either Perl version 5.8.5 or, at your
#   option, any later version of Perl 5 you may have available.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE.
#

=head1 NAME

Algorithm::Pair::Best - Perl module to select pairings (designed for Go
tournaments, but can be used for anything, really).

=head1 SYNOPSIS

    use Algorithm::Pair::Best;

    my $pair = Algorithm::Pair::Best->new( ? options ? );

    $pair->add( item, ? item, ... ? );

    @pairList = $pair->pick( ? $window ? );

=head1 DESCRIPTION

After creating an Algorithm::Pair::Best-E<gt>B<new> object, B<add> a list of
items (players) to be paired.  B<add> connects the new items into a linked
list.  The linked list must consist of an even number of items or you'll get
an error when you try to B<pick> the pairs.

Pairings are determined partially by the original order items were added, but
more importantly, items are paired based on scores which are determined by an
B<info> hash used to attach any random data to the item, and user supplied
functions to provide a score for each item in relation to other items.  It
may be convenient to add access methods to the Algorithm::Pair::Best package
from the main namespace (see the scoreSubs option to B<new> below for an
example).

Algorithm::Pair::Best-E<gt>B<pick> explores all combinations of items and
returns the pairing with the best (highest) score.  This can be an expensive
proposition - the number of combinations goes up very fast with respect to the
number of items:

    items combinations
      2         1       (1)
      4         3       (1 * 3)
      6        15       (1 * 3 * 5)
      8       105       (1 * 3 * 5 * 7)
     10       945       (1 * 3 * 5 * 7 * 9
     12     10395       (1 * 3 * 5 * 7 * 9 * 11)
     14    135135       (1 * 3 * 5 * 7 * 9 * 11 * 13)

It is clearly unreasonable to try to pair a significant number of items.  On
my system it takes about 2 seconds to pair 12 items (6 pairs), and 20 seconds
to pair 14 items (with no 'negative scores only' optimization).  Trying to
completely pair even 30 items would take too long.

Fortunately, there is a way to get pretty good results for large numbers, even
if they're not perfect.  Instead of trying to pair the whole list at once,
Algorithm::Pair::Best-E<gt>B<pick> pairs a series of smaller groups to get
good 'local' results.  The B<new> method accepts a B<window> option to limit
the number of pairs in each window.  The B<window> option can also be
overridden by calling B<pick> with an explicit window argument:

    $pair->pick($window);

See the description of the B<window> option below.

=cut

# first, some boilerplate:
use strict;
require 5.001;


# an Algorithm::Pair::Best is a pairing object.  it is one member of a linked list of
#    Algorithm::Pair::Best's.  The first Algorithm::Pair::Best in the list is
#    a bit different - it's the root and it has some higher-level control
#    functions.  The root is not itself a pairing item.  Items to be paired
#    are 'add'ed to the root Algorithm::Pair::Best object.
package Algorithm::Pair::Best;
use Carp;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use PackageName ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
);

BEGIN {
    our $VERSION = sprintf "%d.%03d", '$Revision: 1.10 $' =~ /(\d+)/g;
}

# uncomment the following line if perl -d is giving you recursion warnings:
# $DB::deep = 1000;     # potentially deep recursion here

#
#  Class Variables
#

my $state = 'init';             # current state
my @items = ();                 # the items to pair
my $bestScore = -999999;        # current best score of all attempts to date
my @bestPairs = ();             # pairing array that yeilded the bestScore
my $currScore;                  # score of current pairing attempt
my @currPairs = ();             # pairing array of current attempt
my $window = 5;                 # pick 5 pairs by default - should finish fairly quickly
my $negOnly = 1;                # for 'negatives scores only' optimization
my $scoreSubs = [ sub { croak "No scoreSubs defined"; } ]; # suitability subroutines used to score candidates
# a hash with the names and default values for object data
my %fields = (
    # class variables common to the whole list of Algorithm::Pair::Best:
    state      => \$state,
    items      => \@items,
    bestScore  => \$bestScore,
    bestPairs  => \@bestPairs,
    currScore  => \$currScore,
    currPairs  => \@currPairs,
    window     => \$window,
    negOnly    => \$negOnly,
    scoreSubs  => \$scoreSubs,
    # normal object data
 #  info       => {},           # user info (ref to empty hash)
 #  citems     => [],           # candidate items, sorted by suitability
 #  cscores    => {},           # scores for each candidate
    opp        => undef,        # the guy we're paired against, or undef if none
    next       => undef,        # the next player in the list, or undef if at the end
    );

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $pair = B<Algorithm::Pair::Best-E<gt>new>(?options?)

A B<new> Algorithm::Pair::Best object becomes the root of a linked list of
Algorithm::Pair::Best objects.  This root does not represent an item to be
paired.  It's just a control point for the collection of items to be paired.

Items are added to the Algorithm::Pair::Best list with the <add> method (see
below).

=head2 Options

=over 4

=item B<window> => number of pairs

Sets the default number of pairs in the sliding pairing window during a
B<pick>.  Can also be set by passing a B<window> argument to B<pick>.

Here's how a window value of 5 (pairs) works:  first pair items 1 through 10.
Keep the pairing for the top two items and then pair items 2 through 12.  Keep
the top pairing and move down to items 4 through 14.  Keep sliding the window
down until we reach the last 10 items (which are completed in one iteration).
In this way, a tournament with 60 players takes less than 1/4 a minute (again,
on my system) to pair with very good results.  See the B<gopair> script in
B<Games::Go::AGA> for a working example.

Default: 5

=item B<negOnly> => true or false

Enable/disable the 'negative scores only" optimization.  If any score greater
than 0 is found during B<sortCandidates>, Algorithm::Pair::Best turns this
flag off.

IMPORTANT: If this flag is turned and a scoreSub can return a number greater
than 0, the resultant pairing may not be optimal, even locally.

Default: 1 (enabled)

=item B<scoreSubs> => reference to array of scoring subroutines

Scoring subroutines are called in array order as:

    foreach my $s (@{$my->scoreSubs}) {
        $score += $my->$s($candidate);
    }

Scores are accumulated and pairings are attempted.  The pairing with the
highest cumulative score is kept as the 'best'.  Note: Algorithm::Pair::Best
works best with scoring subroutines that return only scores less than or equal
to 0 - see the B<sortCandidates> method for more details.

The scoring subroutines should be symmetric so that:

    $a->$scoreSub($b) == $b->$scoreSub($a)


Example:

Note that the scores below are negative (Algorithm::Pair::Best searches for
the highest combined score).  'Negative scores only' allows an optimization
that is probably worth keeping in mind - it can reduce pairing time by several
orders of magnitude (or allow a larger B<window>).  See the B<sortCandidates>
method for more information.

 .  .  .
 # create an array of scoring subroutines:
 our @scoreSubs = (
    sub { # difference in rating.
        my ($my, $candidate, $explain) = @_;

        # the multiplier here is 1, so that makes this the 'normal' factor
        my $score = -(abs($my->rating - $candidate->rating));
        return sprintf "rating:%5.1f", $score if ($explain);
        return $score;
    },
    sub { # already played?
        my ($my, $candidate, $explain) = @_;

        my $already = 0;
        foreach (@{$my->{info}{played}}) {
            $already++ if ($_ == $candidate);       # we might have played him several times!
        }
        # large penalty for each time we've already played
        my $score = -16 * $already;
        return sprintf "played:%3.0f", $score if ($explain);
        return $score;
    },
 );

 # the 'difference in rating' scoring subroutine above needs a 'rating'
 # accessor method in the Algorithm::Pair::Best namespace:
 {
     package Algorithm::Pair::Best;
     sub rating { # add method to access ratings (used in scoreSubs above)
        my $my = shift;

        $my->{info}{rating} = shift if (@_);
        return $my->{info}{rating};
     }
 }
 # back to the main namespace
 .  .  .

In the above example, note that there is an extra optional B<$explain>
argument.  Algorithm::Pair::Best never sets that argument, but user code can
include:

    my @reasons;
    foreach my $sSub (@scoreSubs) {
        push(@reasons, $p1->$sSub($p2, 1));        # explain scoring
    }
    printf "%8s vs %-8s %s\n", $id1, $id2, join(', ', @reasons);

to explain how $p1 scores when paired with $p2.

Default: ref to empty array

=back

=cut

sub new {
    my ($proto, %args) = @_;

    my $my = { %fields };       # copy of default fields
    $my->{info}    = {};        # user info
    $my->{citems}  = [];        # candidate items, sorted by suitability
    $my->{cscores} = {};        # cache of candidate scores
    bless($my, ref($proto) || $proto);
    # transfer user args
    foreach my $a (keys(%args)) {
        if ($a eq 'state') {
            $my->state($args{$a});
        } elsif ($a eq 'items') {
            $my->items(@{$args{$a}});
        } elsif ($a eq 'bestScore') {
            $my->bestScore($args{$a});
        } elsif ($a eq 'bestPairs') {
            $my->bestPairs(@{$args{$a}});
        } elsif ($a eq 'window') {
            $my->window($args{$a});
        } elsif ($a eq 'negOnly') {
            $my->negOnly($args{$a});
        } elsif ($a eq 'scoreSubs') {
            $my->scoreSubs($args{$a});
        } elsif (($a eq 'info') or
                 ($a eq 'citems') or
                 ($a eq 'cscores') or
                 ($a eq 'opp') or
                 ($a eq 'next')) {
            $my->{$a} = $args{$a};
        } else {
            croak "$a is not a legal option to Algorithm::Pair::Best->new";
        }
    }
    return $my;
}

=item Accessor Methods

Accessor methods can read and write the following items:

=over 4

=item B<items>       reference to the list of B<add>ed items (root only)

=item B<info>        reference to the user-defined info hash

=item B<cscores>     reference to the hash of scores cache

=item B<citems>      reference to list of candidates sorted by score

=item B<opp>         currently selected opponent, or undef if not paired

=item B<next>        next candidate in the list

=item B<window>      (class) default number of pairs in sliding window

=item B<negOnly>     (class) use 'negative scores only' optimization

=item B<scoreSubs>   (class) user-supplied list of scoring subroutines

=item B<bestScore>   (class) current best score for all pairings to date

=back

Accessor methods set the appropriate variable if called with a parameter, and
return the current (or new) value.

=cut

# accessor methods for class variables
sub state     {my $my = shift; ${$my->{state}}     = shift if (@_); return ${$my->{state}}    ;}
sub items     {my $my = shift; @{$my->{items}}     = @_    if (@_); return @{$my->{items}}    ;}
sub bestPairs {my $my = shift; @{$my->{bestPairs}} = @_    if (@_); return @{$my->{bestPairs}};}
sub bestScore {my $my = shift; ${$my->{bestScore}} = shift if (@_); return ${$my->{bestScore}};}
sub currScore {my $my = shift; ${$my->{currScore}} = shift if (@_); return ${$my->{currScore}};}
sub window    {my $my = shift; ${$my->{window}}    = shift if (@_); return ${$my->{window}}   ;}
sub negOnly   {my $my = shift; ${$my->{negOnly}}   = shift if (@_); return ${$my->{negOnly}}  ;}
sub scoreSubs {my $my = shift; ${$my->{scoreSubs}} = shift if (@_); return ${$my->{scoreSubs}};}
# accessor methods for object data
sub info      {my $my = shift; $my->{info}      = shift if (@_); return $my->{info}     ;}
sub citems    {my $my = shift; $my->{citems}    = shift if (@_); return $my->{citems}   ;}
sub cscores   {my $my = shift; $my->{cscores}   = shift if (@_); return $my->{cscores}  ;}
sub opp       {my $my = shift; $my->{opp}       = shift if (@_); return $my->{opp}      ;}
sub next      {my $my = shift; $my->{next}      = shift if (@_); return $my->{next}     ;}

=item @pair_items = $pair-E<gt>B<add> ( item [ item ...] )

Add an item (or several items) to be paired.  The item(s) can be any scalar,
but it's most useful if it is a reference to a hash that contains some kind of
ID and information (like rating and previous opponents) that can be used to
B<score> this item relative to the other items.

If a single item is added, the return value is a reference to the
Algorithm::Pair::Best object created for the item (regardless of calling
context).

If multiple items are added, the return value is the list of created
Algorithm::Pair::Best objects in array context, and a reference to the list in
scalar context.

Note: the returned pair_items list is not very useful since they have not yet
been paired.

=cut

sub add {
    my $my = shift;

    $my->state('add');
    my @items;
    foreach my $item (@_) {
        my $pairItem = Algorithm::Pair::Best->new(info => $item);
        if (exists($my->{lastItem})) {
            $my->{lastItem}->next($pairItem);   # link new item into chain
        }
        $my->{lastItem} = $pairItem;            # set new last item
        push(@items, $pairItem);
        push(@{$my->{items}}, $pairItem);
        $my->next($pairItem) unless (defined($my->next));
    }
    if (@items > 1) {
        return wantarray ? @items : \@items;
    }
    return $items[0];
}

=item $pair-E<gt>B<score> ( candidate )

Returns the score (as calculated by calling the list of user-supplied
scoreSubs) of the current pairing item relative to the candidate pairing item.

=cut

sub score {
    my ($my, $candidate) = @_;

    return($my->{cscores}{$candidate}) if (defined($my->{cscores}{$candidate}));
    my $score = 0;
    foreach (@{$my->scoreSubs}) {
        $score += $my->$_($candidate);
    }
    $my->cscores->{$candidate} = $score; # cache it
    $candidate->cscores->{$my} = $score; # cache his while we're at it
    return $score;
}


=item $pair-E<gt>B<sortCandidates>

Sort each candidate list for each item.  This also caches the score for each
candidate in each item.

Normally this routine does not need to be called as the B<pick> method calls
B<sortCandidate> before it starts picking.  However, if you would like to modify
candidate scores based on the sorting itself (for example, in the early rounds
of a tournament, you may wish to avoid pairing the best matches against each
other), you can call B<sortCandidates>, and then make scoring adjustments (use
the B<citems> method to get a reference to the sorted list of candidates, then
use $item-E<gt>B<score>($candidate, $newscore) to change the score).  After
changing the score cache, calling the B<pick> method calls B<sortCandidates>
once more which will re-sort based on the new scores cache.

Note: during B<sortCandidates>, the scores are checked for non-negative
values.  If only 0 or negative values are used, the B<pick> method can
optimize by skipping branches that already score below the current best
pairing.  Any scores greater than 0 disable the 'negative scores only'
(B<negOnly>) optimization.

=cut

sub sortCandidates {
    my ($my) = @_;

    foreach my $item (@{$my->{items}}) {
        my @citems = sort( { $item->score($b) <=> $item->score($a) } @{$my->{items}});
        $item->citems(\@citems);
        foreach my $cs (values(%{$item->{cscore}})) {
            ${$my->{negOnly}} = 0 if ($cs > 0);
        }
    }
    $my->state('sort');
}

=item @pairs = $pair-E<gt>B<pick> ( ?$window? )

Returns the best pairing found using the sliding window technique (calling
B<wpick>) as discussed in DESCRIPTION above.  The size of the window is
B<$windows> pairs (2*$windows items).  If no window argument is passed, the
default window selected in the B<new> call is used.

B<pick> returns the list (or a reference to the list in scalar context) of
Algorithm::Pair::Best objects in pairing order: item[0] is paired to item[1],
item[2] to item[3], etc.  

B<pick> performs a sanity check on the pairs list, checking that no item is
paired twice, and that all items are paired.

=cut

sub pick {
    my ($my, $window) = @_;

    $window = $my->window unless(defined($window)); # size of sliding window
    my (@pairs, %sanity);
    my $alreadyPaired = 0;
    my $notPaired = 0;
    while (@pairs < @{$my->{items}}) {
        my $top = $my->wpick($window); # pick top pairs
        my $save = 1;
        if (((@{$top} + @pairs) >= @{$my->{items}}) or
            (@{$top} < 2 * $window)) {
            $save = @{$top} / 2;        # done - empty the list
        }
        while ($save--) {
            my $p1 = shift @{$top};
            my $p2 = shift @{$top};
            $my->progress($p1, $p2) if (defined(&progress));
            $p1->opp($p2);              # take these two out of contention
            $p2->opp($p1);
            push (@pairs, $p1, $p2);
            $alreadyPaired++ if (exists($sanity{$p1}));
            $alreadyPaired++ if (exists($sanity{$p2}));
            $sanity{$p1} = $p2;         # yeah, I know we 'can't usefully use
            $sanity{$p2} = $p1;         #       refs as hash keys', but we
                                        #       don't need the ref back here,
                                        #       just need a unique key
        }
    }
    foreach (@{$my->{items}}) {
        $notPaired++ unless (exists($sanity{$_}));
        delete($sanity{$_});
    }
    my $msg = '';
    $msg .= "$alreadyPaired ITEMS ALREADY PAIRED!\n" if($alreadyPaired);
    $msg .= "$notPaired ITEMS NOT PAIRED!\n" if($notPaired);
    $msg .= scalar(keys(%sanity)) . " ITEMS EXTRA!\n" if(scalar(keys(%sanity)));
    croak "$msg" unless ($msg eq '');
    return wantarray ? @pairs : \@pairs;
}

=item $pair-E<gt>B<progress> ( $item0, $item1 )

Each time a pair is finalized in the B<pick> routine above, it checks to see
if a subroutine called B<progress> has been defined.  If so, B<pick> calls
$pair->B<progress>($item0, $item1) where $item0 and $item1 are the most
recently added pair of items.

B<progress> is not defined in the Algorithm::Pair::Best package.  It is meant
to be provided by the caller.  For example, to print a message as each pair is
finalized:

 .  .  .
 {
     package Algorithm::Pair::Best;
     sub progress {
        my ($my, $item0, $item1) = @_;

        # assuming you have provided an 'id' method that returns a string:
        print $item0->id, " paired with ", $item1->id, "\n";
     }
 }

 # back to main:: namespace
 .  .  .

=cut

=item $pairsRef = $pair-E<gt>B<wpick> ( $window )

Normally B<wpick> is only called by the B<pick> method.

B<wpick> returns a reference to a list of the best pairing of B<$window> pairs
(or 2*B<$window> items) starting from the first unpaired item in the list (as
determined by B<add> order).  The returned list is in pairing order as
described in B<pick>.

If there are fewer than 2*B<$window> items remaining to be paired, prints an
error and returns the best pairing for the remaining items.  If an odd number
of items remain, prints an error and returns the best pairing excluding the
last item.

Note that while the pairing starts from the first item in the B<add> list, the
returned pairs list may contain items from outside the first 2*B<$window> items
in the B<add> list.  This is because each item has its own ordered list of
preferred pairs.  However, the first unpaired item in the B<add> list will be
the first item in the returned list.

Similarly, in the 'odd number of items remaining' situation, the discarded
item is not neccessarily the last item in the B<add> list.

=cut

sub wpick {
    my ($my, $window) = @_;

    unless($my->state eq 'pick') {
        $my->sortCandidates;
    }
    $my->state('pick');
    my $avail = 0;
    foreach my $p (@{$my->{items}}) {
        next if (defined($p->opp));     # skip if already paired
        $my = $p if($avail == 0);       # make my be first available
        $avail++;
        last if ($avail >= $window * 2);
    }
    if ($avail <= 1) {
        if ($avail == 1) {
            carp "Can't pick 1 - returning empty pairing list (last player will be left out)";
        }
        return ([]);
    }
    if ($avail % 2) {
        $avail--;
        carp "Can't pick from an odd number - last player will be left out";
    }

    $avail /= 2;
    if ($avail < $window) {
        carp "Not enough candidates: reducing maxPair to $avail";
    }

    $my->bestScore(-999999);
    @{$my->{bestPairs}} = ();
    $my->currScore(0);
    @{$my->{currPairs}} = ();
    # kick off recursive pairing
    $my->_rpick($my->next, $avail, 0);        # starting candidate is my next, and depth is 0
    if ($my->{oddError}) {      # shouldn't be possible
        carp STDERR "Warning: odd number, last player not paired!\n"
    }
    return($my->{bestPairs});
}

=item $score = $pair-E<gt>B<scoreFunc> ( $candidate, $score )

B<scoreFunc> is not defined in the Algorithm::Pair::Best package, but the
B<pick> method checks to see if the caller has defined a subroutine by that
name.  If defined, it is called each time a candidate score is added to the
B<currScore> total for a trial pairing.

Normally, Algorithm::Pair::Best simply adds the scores and tries for the
highest total score.  Some pairings may work better with a different total
score, for example the sum of the squares of the scores (to reduce the ability
of one bad pairing to compensate for a group of good pairings).  B<scoreFunc>
provides a hook for this modification.

If defined, scoreFunc is called as:

    $score = $item->scoreFunc($candidate, $score);

where $item is the current Algorithm::Pair::Best item being paired, $candidate
is the current candidate item under consideration, and $score is $candidate's
unaltered score (wrt $item).

IMPORTANT: Remember to retain negative scores (or disable the B<negOnly>
optimization.

Example use of B<scoreFunc>:
 .  .  .
 {
     package Algorithm::Pair::Best;
     sub scoreFunc {
        my ($my, $candidate, $score) = @_;

        # we want to minimize the squares of the scores:
        return -($score * $score);
     }
 }

 # back to main:: namespace
 .  .  .

=cut

sub _rpick {
    my ($my, $firstCandidate, $window, $deep) = @_;

    # number of candidates to try to pair with - any more
    #    just eats time without improving the quality of the pairing
    my $maxCand = (2 * $window) - 1;
    push(@{$my->{currPairs}}, $my);             # put myself on the pairing list
    my $cIdx = 0;
    my $candidate;
    for (my $ii = 0; $ii < $maxCand; $ii++) {
        do {
            $candidate = $my->citems->[$cIdx++];
        } while (($cIdx <= scalar(@{$my->citems})) and
                 (defined($candidate->opp) or   # find next unpaired in candidate list
                  ($candidate == $my)));        # make sure it's not me!
        last unless ($cIdx <= scalar(@{$my->citems}));
        my $cscore = $my->score($candidate);
        # call callers scoreFunc if defined
        $cscore = $my->scoreFunc($candidate, $cscore) if(defined(&scoreFunc));
        ${$my->{currScore}} += $cscore;         # add opp's score to total
        if (${$my->{negOnly}} and (${$my->{currScore}} < ${$my->{bestScore}})) {
            # we're below best, and it can only get worse, skip!
        } else {
            $candidate->opp($my);               # pair him with me
            $my->opp($candidate);               # and me with him
            push(@{$my->{currPairs}}, $candidate); # put this opp on the pairing list
            if ($window > 1) {
                my $next = $my;
                while (defined($next) and defined($next->opp)) {
                    $next = $next->next;
                }
                if (defined($next)) {
                    $next->_rpick($firstCandidate, $window - 1, $deep + 1);
                } else {
                    carp "Out of candidates too soon!"; # shouldn't happen
                }
            } else {
                # choose best of previous or current:
                if (${$my->{currScore}} > ${$my->{bestScore}}) {
                    ${$my->{bestScore}} = ${$my->{currScore}};
                    @{$my->{bestPairs}} = ();
                    @{$my->{bestPairs}} = @{$my->{currPairs}};
                }
            }
            pop(@{$my->{currPairs}});           # remove opp from pairing list
            $candidate->opp(undef);             # break pairing with this guy
        }
        ${$my->{currScore}} -= $cscore;         # remove opp's score to total
    }
    pop(@{$my->{currPairs}});                   # remove me from current list
    $my->opp(undef);                            # I ain't got nobody...
    return;
}

1;

__END__

=back

=head1 SEE ALSO

=over 0

=item o gopair(1)

The B<gopair> script from the Games::Go::GoPair package uses
Algorithm::Pair::Best to run pairings for a go tournament

=back

=head1 AUTHOR

Reid Augustin, E<lt>reid@HelloSix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004, 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut

