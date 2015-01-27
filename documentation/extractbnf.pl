my $in_prod_list = 0;
my $grammar = 0;
print ".. productionlist::\n";
while(<>) {
  if (m/^.. start grammar$/) {
    $grammar = 1;
  }
  elsif ($grammar) {
    if (m/^.. productionlist::$/) {
      $in_prod_list = 1;
    }
    elsif ($in_prod_list) {
      if (m/^\s*$/) {
        $in_prod_list = 0;
      }
      else {
        print;
      }
    }
  }
}
