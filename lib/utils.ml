open Core
open Bistro
open Bistro.Shell_dsl


(* process substitution for gunzip *)
let psgunzip x =
  seq ~sep:"" [ string "<(gunzip -c " ; dep x ; string ";)" ]


let select_contig_script = {|#!/usr/bin/perl

#3sept2014: trinity version as of 2014, as changed the contig formats; this script should now handle both format

#2Feb2016: new trinity format and kallisto

use strict;
use warnings;
use Bio::SeqIO;
use Getopt::Long;
#use YAML::XS;


my $byexp;
my $bylength = 1;

GetOptions('byexp=s' => \$byexp);

my $help = <<HELP;
Usage: $0 <trinity assembly> <fasta output>
Select one sequence per trinity component. If a
transcript as several ORFs (several ORF within the same
contig), only the longuest one is kept.
Options:
	-bylength [default]
	-byexp <RSEM or kallisto isoform expression file>
HELP

if($#ARGV < 1) {
    print $help;
    exit;
}

#open YAML, ">temp.yaml";

my $seqin = Bio::SeqIO->new(-file => "$ARGV[0]", -format => 'fasta');
my $seqout = Bio::SeqIO->new(-file => ">$ARGV[1]", -format => 'fasta');

my %seenSeq;
my %big;
my $format;
my $n = 0;
my %orf;
while(my $seq = $seqin->next_seq) {
	++$n;
    my $id = $seq->id;
#     print "$id\n";
#   #test the format
	if($n == 1) {
		if($id =~ /comp(\d+)_c(\d+)_seq(\d+)/) { $format = 1 }
		elsif($id =~ /TRINITY_(DN\d+_c\d+)_g(\d+)_i(\d+)/) { $format = 4 }
		elsif($id =~ /(?<!\|)c(\d+)_g(\d+)_i(\d+)/) { $format = 2 }
		elsif($id =~ /TR\d+\|c(\d+)_g(\d+)_i(\d+)/) { $format = 3 }
		else {

			print "I can't parse this: $_\n";
			next;
		}
		print "Sequence format is trinity v$format\n";
	}

	my $comp; my $sub; my $sq; my $seqname;
	my $len = $seq->length();

	if($format == 1) {
		$id =~ /(comp(\d+)_c(\d+)_seq(\d+))/;
		$seqname = $1;
		$comp = $2;
	}
	elsif ($format == 2) {
		$id =~ /(c(\d+)_g(\d+)_i(\d+))/;
		$seqname = $1;
		$comp = $2;
	}
	elsif ($format == 3) {
		$id =~ /((TR\d+\|c\d+)_g(\d+)_i(\d+))/;
                $seqname = $1;
		$comp = $2;
	}
	elsif ($format == 4) {
		$id =~ /(TRINITY_(DN\d+_c\d+)_g(\d+)_i(\d+))/;
                $seqname = $1;
		$comp = $2;
	}

	$seenSeq{$seqname} = 1;

	#hash the length anyway to select the longuest ORF per seq
	#unless($byexp) {
		if(exists $big{$comp}) {
			if($big{$comp}{length} < $len) {
				$big{$comp}{length} = $len;
				$big{$comp}{id} = $seqname;
				$big{$comp}{fullid} = $id;
	# 			print "$comp -> $id\n";
			}
		}
		else {
			$big{$comp}{length} = $len;
			$big{$comp}{id} = $seqname;
			$big{$comp}{fullid} = $id;
# 			print "$comp -> $id\n";
		}
   	#}


	#hash the number of contigs per seq and the longuest one
	#this happens when there is several ORF per contig, very rare
	$orf{$seqname}{$id} = $len;
	++$orf{$seqname}{nORF};

	if(exists $orf{$seqname}{longuest}) {
		if($orf{$seqname}{longuest} < $len) {
			$orf{$seqname}{longuest} = $len;
			$orf{$seqname}{longuestSeq} = $id;
		}
	}
	else {
                        $orf{$seqname}{longuest} = $len;
                        $orf{$seqname}{longuestSeq} = $id;
	}
}


#parse RSEM selection is made using expression
my %rsem;

if($byexp) {
	my $formatRsem;
	open(EXPR, $byexp);
	$n = 0;
	while(<EXPR>) {
		++$n;
		#print "$n\n";
		next if $n == 1;

		my @f = split "\t";
		my $id = $f[0];
			#print "$n $id\n";
		my $nreads = $f[3];

		if($n == 2) {
			#print "HERE\n";
			if($id =~ /comp(\d+)_c(\d+)_seq(\d+)/) { $formatRsem = 1 }
			elsif($id =~ /TRINITY_(DN\d+_c\d+)_g(\d+)_i(\d+)/) { $formatRsem = 4 }
                	elsif($id =~ /(?<!\|)c(\d+)_g(\d+)_i(\d+)/) { $formatRsem = 2 }
                	elsif($id =~ /TR\d+\|c(\d+)_g(\d+)_i(\d+)/) { $formatRsem = 3 }
                	else {
                        	die "I can't parse this: $_\n";
                	}
			print "Format of RSEM or Kallisto is $formatRsem\n";
		}

		#do not consider if not in the original list
		next unless exists $seenSeq{$id};

		my $comp;
		if($formatRsem == 1) {
			$id =~ /comp(\d+)_c(\d+)_seq(\d+)/;
			$comp = $1;
		}
		if($formatRsem == 2) {
			$id =~ /c(\d+)_g(\d+)_i(\d+)/;
                	$comp = $1;
                }
 		if($formatRsem == 3) {
			$id =~ /(TR\d+\|c\d+)_g(\d+)_i(\d+)/;
	                $comp = $1;
                }
		if ($formatRsem == 4) {
			$id =~ /(TRINITY_(DN\d+_c\d+)_g(\d+)_i(\d+))/;
			$comp = $2;
		}


		if(exists $rsem{$comp}) {
			if($rsem{$comp}{nreads} < $nreads) {
                        	$rsem{$comp}{nreads} = $nreads;
                       		$rsem{$comp}{id} = $id;
                	}
		}
        	else {
                	$rsem{$comp}{nreads} = $nreads;
                	$rsem{$comp}{id} = $id;
			#print "$comp -> $id\n";
        	}
	}
}
#print YAML Dump %rsem;

my $nexport = 0;
$seqin = Bio::SeqIO->new(-file => "$ARGV[0]", -format => 'fasta');
while(my $seq = $seqin->next_seq) {
	my $id = $seq->id;
	my $comp; my $seqname;
# 	print "$id\n";
	if($format == 1) {
		$id =~ /(comp(\d+)_c(\d+)_seq(\d+))/;
		$seqname = $1;
		$comp = $2;
	}
	elsif($format == 2) {
		$id =~ /(c(\d+)_g(\d+)_i(\d+))/;
                $seqname = $1;
                $comp = $2;

	}
	elsif ($format == 3) {
                $id =~ /((TR\d+\|c\d+)_g(\d+)_i(\d+))/;
                $seqname = $1;
		$comp = $2;
        }
	elsif ($format == 4) {
		$id =~ /(TRINITY_(DN\d+_c\d+)_g(\d+)_i(\d+))/;
                $seqname = $1;
		$comp = $2;
	}


	if($byexp) {
		unless(exists $rsem{$comp}) {
			print "Can't find the expression of $comp\n";
			next;
		}
		if($rsem{$comp}{id} eq $seqname) {
			#test if there is several ORF for this sed, if yes, only export the longuest
			if($orf{$seqname}{nORF} > 1) {
				next unless $orf{$seqname}{longuestSeq} eq $id;
			}
                	$seqout->write_seq($seq);
			++$nexport;
                }

	}
	else {
		if($big{$comp}{id} eq $seqname) {
			if($orf{$seqname}{nORF} > 1) {
                                next unless $orf{$seqname}{longuestSeq} eq $id;
                        }
			$seqout->write_seq($seq);
			++$nexport;
		}
	}
}

print "Written $nexport sequences\n";|}


let env = docker_image ~account:"pveber" ~name:"bistro-base" ~tag:"jessie" ()

let select_contig abundance fa =
  Workflow.shell ~descr:"trinity_asb_to_one_contig_per_comp.pl" [
    mkdir_p tmp ;
    cmd "perl" ~env [
      file_dump (string select_contig_script) ;
      opt "-byexp" dep abundance ;
      dep fa ;
      dest
    ] ;
  ]

let rename_contigs species fa =
  Workflow.shell ~descr:"rename species" [
    cmd "sed" ~stdout:dest [
      string (sprintf {|"s/^>\(.*\)$/>%s_\1/"|} species) ;
      dep fa
    ]
  ]
