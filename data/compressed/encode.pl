#!/usr/bin/perl
# Read in a schema file containing field names and possible values, and a data file.
# Replaces non-numeric field values with a numeric value (starting from 0).

# Data file should be a bunch of records, one per line. Commas separate fields,
# spaces are important. Each line should be ended with a period.
#
# Schema file should be a list of fields, of the form
#
# <name>: continuous.
#
# if a continuous field, or
#
# <name>:<v1>,<v2>,...<vn>.
#
# if a discrete field with n possible values v1 ... vn. Note spaces are
# important, and note the period at the end of each field specification.

if ($#ARGV != 2) {
  print "Usage: ./encode.pl <schema_file> <data_file> <output_file>\n";
  exit 1;
}


$schema_file = shift;
$data_file = shift;
$output_file = shift;

open(SCHEMA, "<$schema_file");
open(DATA, "<$data_file");
open(OUTPUT, ">$output_file");

# Parse schema
@schema = ();

print OUTPUT "===================================\n";
print OUTPUT "FIELDS\n";
print OUTPUT "===================================\n\n";
while (<SCHEMA>) {
  $mapping = {};
  if ($_ =~ '^(.*):(.*)\.') {
    $name = $1;
    if ($_ !~ "continuous") {
      @fields = split(',', $2);
      $numvalues = $#fields + 1;
      print OUTPUT "$name: discrete ($numvalues values).\n";
      for my $i (0 .. $#fields) {
        $mapping->{$fields[$i]} = $i;
#       print "Mapping $fields[$i] to $i\n";
      }
    } else {
      print OUTPUT "$name: continuous.\n";
    }
  }
  push(@schema, $mapping);
}

print OUTPUT "\n\n===================================\n";
print OUTPUT "DATA\n";
print OUTPUT "===================================\n\n";

close SCHEMA;

# Process data file, relabel non-numeric values.

while (<DATA>) {
  /(.*)\./ and @values = split(',', $1);
  $cur = '';
  for my $i (0 .. $#values) {
    if ( keys %{$schema[$i]} ) {
      $label = $schema[$i]->{$values[$i]};
      # print "Processing value $values[$i], found label $label\n";
      if ($cur eq '') {
        $cur = $label;
      } else {
        $cur = $cur . ", " . $label;
        # print "lo: $cur\n";
      }
    } else {
      # No label mapping, must be continuous variable
      # print "Processing value $values[$i], continuous\n";
      if ($cur eq '') {
        $cur = $values[$i];
      } else {
        $cur = $cur . ", " . $values[$i];
      }
    }
  }
  print OUTPUT ($cur . ".\n");
}

close DATA;
close OUTPUT;
