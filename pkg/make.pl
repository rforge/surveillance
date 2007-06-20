#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;

my $basePath = "../surveillance/";
my $manPath  = "$basePath/man/";
my $RPath = "$basePath/R/";

my $R_HOME = "";
my $R_LIBS = "";
my $Rterm = "";
my $RCMD = "";
my $CheckCall = "";
my $InstallCall = "";
my $ManualCall = "";
my $BinaryCallWin = "";
my $BinaryCallLin = "";

print "$^0\n";

#If Unix or DOS
if($^O eq "linux"){
	#Make paths UNIX compatible
	$R_HOME = $ENV{"R_HOME"};
	# cut the last '/'
	$R_HOME =~ s/\/$//;

	$Rterm = "R";
	$RCMD = "R CMD";
	$CheckCall = "$RCMD check .";
	$InstallCall = "$RCMD INSTALL $basePath";
}
else{ # for windows
	$R_HOME = $ENV{"R_HOME"};
	$R_HOME =~ s/\\/\\\\/g;
	$R_LIBS = $ENV{"R_LIBS"}; 
	$R_LIBS =~ s/\\/\\\\/g;
#	$Rterm = $R_HOME . "bin/Rterm.exe"; #R
	$Rterm = "R "; 
#	$RCMD = $R_HOME . "bin/Rcmd.exe"; 
	$RCMD = "R CMD "; 
	$CheckCall = "$RCMD check .";
	$ManualCall = "$RCMD Rd2dvi --pdf --title=\"Appendix: The package surveillance\" --no-clean $basePath";
#	$InstallCall = "$RCMD INSTALL --docs=normal -l $R_HOME/library $basePath";
	$InstallCall = "$RCMD INSTALL --docs=normal -l $R_LIBS/ $basePath";
	$BinaryCallWin = "$RCMD build --force --binary $basePath; mv *.zip .. ;$ RCMD build $basePath ; mv *.tar.gz ..";
#	$BinaryCallLin = "$RCMD build --force $basePath;$RCMD build $basePath";
#	$BinaryCallLin = "cmd.exe /c c:\\MyProgramme\\R\\rw2001\\bin\\Rcmd build ../surveillance";
}

our $opt_help = 0;
our $opt_clean = 0;
our $opt_check = 0;
our $opt_install = 0;
our $opt_manual = 0;
our $opt_binary = 0;

######################################################################
# Process options
######################################################################
my @knownoptions = ("help|h", "clean", "install|i", "check|c", 
		    "manual|m", "binary|b");
GetOptions (@knownoptions) || usage();
usage() if $opt_help;
clean() if $opt_clean;

# Run through all .Rnw
for(my $i = 0; $i < @ARGV; $i++){

	my $RnwFile  = $ARGV[$i];
	my $helpFile = $RnwFile;
	my $RFile = $RnwFile;
	#Always remove the Rnw folder name for the file name
	#print($helpFile . "\n");
        $helpFile =~ s/\.Rnw/\.tex/;
        $helpFile =~ s/Rnw\///;
	#print($helpFile . "\n");
        $RFile =~ s/\.Rnw/\.R/;
        $RFile =~ s/Rnw//;

	######################################################################
	#Run Stangle
	######################################################################
	print "Running Stangle on $RnwFile...\n";
	my @Stangle = `echo "library(tools); Stangle(\\"$RnwFile\\")" | $Rterm --no-save --no-restore`;
	# print @Stangle;

	######################################################################
	#Run Sweave
	######################################################################
	print "Running Sweave on $RnwFile...\n";
	my @Sweave = `echo "library(tools); Sweave(\\"$RnwFile\\")" | $Rterm --no-save --no-restore`;
	# print @Sweave;

	######################################################################
	#Make RD files
	######################################################################
	print "Making RD files on $RnwFile...\n";

	open(HELPALL,"<$helpFile") || die "Cannot open $helpFile.\n";

	#Loop over all lines
	my $active = 0;
	while (<HELPALL>){
	my $line = $_;
	#Search for \name{...}
	if ($line=~/^\\name\{([\w|\.]+)\}.*/) {
		my $fileName = "$manPath$1.Rd";
		print $fileName . "\n";

		#If we were writing files it has to be closed
		if ($active) { close(ONE); }

		#Open the new file
		$active = 1;
		open(ONE,">$fileName") || die ("Cannot open the help file $fileName\n");
	}
	#If we have seen a \name then we are writing
	if ($active) {
		print ONE $line;
	}
	}

	if ($active) {close(ONE);$active = 0;}

	######################################################################
	#Move .R file and Clean
	######################################################################
	print "Cleaning...\n";

	`mv $basePath/$RFile $RPath`;
	`rm $basePath/$helpFile`;

} # end for

check() if $opt_check;
manual() if $opt_manual;
install() if $opt_install;
binary() if $opt_binary;



######################################################################
# Clean directory tree
######################################################################

sub clean {
    print "Cleaning directory tree...\n";

    system("rm -r *.Rcheck");
    system("rm -r ./R/*.R");
    system("rm -r ./man/*.Rd");
}


######################################################################
# Check package
######################################################################

sub check {
    print "Checking package...\n";

    system($CheckCall);
}


######################################################################
# Make manual
######################################################################

sub manual {
    print "R2D2...\n";

    system($ManualCall);
}

######################################################################
# Install package
######################################################################

sub install {
    print "Installing package...\n";

    system($InstallCall);
}

######################################################################
# Create binaries
######################################################################

sub binary {
    print "Creating binaries...\n";

    system($BinaryCallWin);
#    system($BinaryCallLin);
}


######################################################################
# Usage help
######################################################################
sub usage {
    print STDERR <<END;
Usage: genRD.pl RnwFiles [options]

Process the .Rnw file extracting the R source code and the Rd files.
Rd files are copied to the man folder.


Options:
  -h, --help		print short help message and exit
  --clean		cleaning the directory tree (checkFiles, *.R, *.Rd)
  -c, --check		check the package
  -m, --manual          create the manual
  -i, --install		install the package using R CMD or Rcmd - uses \$R_HOME
  -b, --binary          create the zip and tar.gz files

Report bugs to <hoehle\@stat.uni-muenchen.de>.
END
    exit 0;
}

# c:/MyProgramme/R/rw2001/bin/Rcmd.exe build --binary .
