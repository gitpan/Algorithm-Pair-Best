use 5.001;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    NAME              => 'Algorithm::Pair::Best',
    VERSION_FROM      => 'Best.pm', # finds $VERSION
    PREREQ_PM         => {
                          }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'Best.pm', # retrieve abstract from module
       AUTHOR         => 'Reid Augustin <reid@HelloSix.com>') : ()),
);