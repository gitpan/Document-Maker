package Document::Maker;

use warnings;
use strict;

=head1 NAME

Document::Maker - Document::Maker

=head1 VERSION

Version 0.01_02

=cut

our $VERSION = '0.01_02';

=head1 SYNOPSIS

=cut

use Path::Class;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/rule_lst built/);

sub new {
	my $self = bless {}, shift;
	$self->rule_lst([]);
	$self->built({});
	return $self;
}

sub build {
	my $self = shift;
	my $tgt = shift;

#	return if $self->built->{$tgt};

	my @rule_lst = @{ $self->rule_lst };
	for my $rule (@rule_lst) {
		if ($rule->can_build($tgt)) {
			my $mtime = $rule->build($tgt);
			$self->built->{$tgt} = 1;
			return $mtime;
		}
	}

    my $tgt_file = file $tgt;
    return $tgt_file->stat->mtime if -e $tgt_file;
    return 0;

#	die "Don't know how to build $tgt";
}

sub rule {
	my $self = shift;
	my $rule =  Document::Magick::Maker::Rule->new(@_, maker => $self);
	push @{ $self->rule_lst }, $rule;
	return $rule;
}

package Document::Magick::Maker::Rule::Map;

use strict;
use warnings;

use Path::Class;
use Carp;
use Scalar::Util qw/blessed/;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/rule _src_lst _tgt_lst _src_pattern _tgt_pattern _need_lst/);

sub new {
	my $self = bless {}, shift;
	local %_ = @_;
	$self->_tgt_lst(ref $_{tgt_lst} eq "ARRAY" ? $_{tgt_lst} : [ $_{tgt_lst} ]) if defined $_{tgt_lst};
	$self->_tgt_pattern($_{tgt_pattern});
	$self->_src_pattern($_{src_pattern});
	$self->_src_lst(ref $_{src_lst} eq "ARRAY" ? $_{src_lst} : [ $_{src_lst} ]) if defined $_{src_lst};
	$self->_need_lst(ref $_{need_lst} eq "ARRAY" ? $_{need_lst} : [ $_{need_lst} ]) if defined $_{need_lst};
    return $self;
}

sub _template_from_pattern($) {
    my $pattern = shift;

    my $template = $pattern;
    if ($template =~ m/%\(.*\)/) {
        $template =~ s/%\((.*)\)/%/;
    }

    return $template;
}

sub _matcher_from_pattern($) {
    my $pattern = shift;

    # TODO Don't descend into . directories?

    my $matcher = $pattern;
    if ($matcher =~ m/%\(.*\)/) {
        $matcher =~ s/%\((.*)\)/($1)/;
    }
    elsif ($matcher =~ m{\%\*}) {
		$matcher =~ s/\%\*/(.*)/;
    }
    elsif ($matcher =~ m{\%\%}) {
		$matcher =~ s/\%/([^\.\/][^\/]*)/;
    }
    elsif ($matcher =~ m{.*/.*\%}) {
		$matcher =~ s/\%/(.*)/;
    }
    else {
		$matcher =~ s/\%/(?:^|\/)([^\.\/][^\/]*)/;
    }

    return qr/$matcher/;
}

sub _name_from($$) {
    my $in = shift;
    my $pattern = shift;
    return $in unless defined $pattern;
    my $matcher = _matcher_from_pattern $pattern;
    my ($out) = $in =~ $matcher;
    return $out;
}

sub _from_name($$) {
    my $name = shift;
    my $pattern = shift;
    return undef unless defined $pattern;
    my $template = _template_from_pattern $pattern;
    my $out = $template;
    $out =~ s/\%/$name/;
    return $out;
}

sub name_from_tgt {
	my $self = shift;
	my $tgt = shift;
	return return _name_from $tgt, $self->_tgt_pattern;
}

sub tgt_from_name {
	my $self = shift;
	my $name = shift;
	return _from_name $name, $self->_tgt_pattern;
}

sub name_from_src {
	my $self = shift;
	my $src = shift;
	return return _name_from $src, $self->_src_pattern;
}

sub src_from_name {
	my $self = shift;
	my $name = shift;
	return _from_name $name, $self->_src_pattern;
}

sub src_matcher {
	my $self = shift;
    return _matcher_from_pattern $self->_src_pattern;
}

sub tgt_lst {
	my $self = shift;

	my @tgt_lst;
	push @tgt_lst, @{ $self->_tgt_lst } if $self->_tgt_lst;

	my @src_lst;
	@src_lst = @{ $self->_src_lst } if $self->_src_lst;
	for my $src (@src_lst) {
        if (blessed $src && $src->isa("Path::Class::Dir") || $src =~ s/~$//) {
            my $src_dir = dir $src;
            next unless -d $src_dir;
            my $src_matcher = $self->src_matcher;
            for ($src_dir->children) {
                next unless $_ =~ $src_matcher;
#               next if $self->src_exclude && $_ =~ $self->src_exclude;
                my $name = $self->name_from_src($_);
                my $tgt = $self->tgt_from_name($name);
                push @tgt_lst, [ $tgt, $_, $self ];
            }
        }
        else {
	        my $name = $self->name_from_src($src);
            my $tgt = $self->tgt_from_name($name);
            push @tgt_lst, $tgt;
        }
	}

	return @tgt_lst;
}

sub can_build {
	my $self = shift;
	my $tgt = shift;
	
	my @tgt_lst = $self->tgt_lst;
	return scalar grep { $tgt eq $_ } @tgt_lst;
}

sub build_all {
	my $self = shift;
	my $rule = shift;

	my @tgt_lst = $self->tgt_lst;

	for my $tgt (@tgt_lst) {
		$rule->build($tgt);
    }
}

package Document::Magick::Maker::Rule;

use strict;
use warnings;

use Path::Class;
use Carp;
use Scalar::Util qw/blessed/;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/maker _map_lst _do label/);

sub new {
	my $self = bless {}, shift;
    my @map_lst;
    if (ref $_[0] eq "HASH") {
        while (@_) {
            last unless ref $_[0] eq "HASH";
            my $map = shift @_;
            push @map_lst, Document::Magick::Maker::Rule::Map->new(%$map);
        }
    }
    elsif (ref $_[0] eq "ARRAY") {
        while (@_) {
            last unless ref $_[0] eq "ARRAY";
            my $map = shift @_;
            push @map_lst, Document::Magick::Maker::Rule::Map->new(
                tgt_lst => $map->[0], tgt_pattern => $map->[1],
                src_pattern => $map->[2], src_lst => $map->[3],
                need_lst => $map->[4],
            );
        }
    }
    else {
	    local %_ = @_;
        push @map_lst, Document::Magick::Maker::Rule::Map->new(
            tgt_lst => delete $_{tgt_lst}, tgt_pattern => delete $_{tgt_pattern},
            src_lst => delete $_{src_lst}, src_pattern => delete $_{src_pattern},
            need_lst => delete $_{need_lst},
        );
    }
    local %_ = @_;
	$self->_map_lst(\@map_lst);
	$self->_do($_{do});
	$self->maker($_{maker});
	$self->label($_{label});
	return $self;
}

sub _find_map_for_tgt {
	my $self = shift;
	my $tgt = shift;

    my @map_lst = @{ $self->_map_lst };
    for my $map (@map_lst) {
        return $map if $map->can_build($tgt);
    }
    return undef;
}

sub build {
	my $self = shift;
	my $tgt = shift;

    if (defined $self->label && $tgt eq $self->label) {
        return $self->build_all;
    }

    my ($src, $name, $map);
	if (ref $tgt eq "ARRAY") {
        # TODO Do we use this?
		($tgt, $src, $map) = @$tgt;

	    $name = $map->name_from_tgt($tgt);
	}
	else {
        $map = $self->_find_map_for_tgt($tgt);

        $name = $map->name_from_tgt($tgt);
        $src = $map->src_from_name($name);
	}

	$tgt = file $tgt;
	$src = file $src if $src;

    if ($src) {
        my $build = 0;

        my @need_lst = @{ $map->_need_lst || [] };
        for my $need (@need_lst) {
	        my $mtime = $self->maker->build($need);
            $build = 1 unless -e $tgt && $tgt->stat->mtime > $mtime;
        }

	    my $mtime = $self->maker->build($src);

#        unless (-e $tgt && $tgt->stat->mtime > (-e $src ? $src->stat->mtime : 0)) {
    
        if ($build || !(-e $tgt && $tgt->stat->mtime > $mtime)) {
            $self->_do->($tgt, $src, $name);
        }

        return $tgt->stat->mtime if -e $tgt;
    }
    else {
        $self->_do->($tgt);
        return 0;
    }

    return 0;
}

sub can_build {
	my $self = shift;
	my $tgt = shift;
	
    return 1 if defined $self->label && $tgt eq $self->label;
    return $self->_find_map_for_tgt($tgt);
}

sub build_all {
	my $self = shift;

	my @map_lst = @{ $self->_map_lst };

	for my $map (@map_lst) {
		$map->build_all($self);
    }
}

=head1 AUTHOR

Robert Krimen, C<< <rkrimen at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-document-maker at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Document-Maker>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Document::Maker

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Document-Maker>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Document-Maker>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Document-Maker>

=item * Search CPAN

L<http://search.cpan.org/dist/Document-Maker>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2007 Robert Krimen, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Document::Maker

__END__

package Document::Magick::Maker;

use strict;
use warnings;

use Path::Class;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/rule_lst built/);

sub new {
	my $self = bless {}, shift;
	$self->rule_lst([]);
	$self->built({});
	return $self;
}

sub build {
	my $self = shift;
	my $tgt = shift;

#	return if $self->built->{$tgt};

	my @rule_lst = @{ $self->rule_lst };
	for my $rule (@rule_lst) {
		if ($rule->can_build($tgt)) {
			my $mtime = $rule->build($tgt);
			$self->built->{$tgt} = 1;
			return $mtime;
		}
	}

    my $tgt_file = file $tgt;
    return $tgt_file->stat->mtime if -e $tgt_file;
    return 0;

#	die "Don't know how to build $tgt";
}

sub rule {
	my $self = shift;
	my $rule =  Document::Magick::Maker::Rule->new(@_, maker => $self);
	push @{ $self->rule_lst }, $rule;
	return $rule;
}

package Document::Magick::Maker::Rule::Map;

use strict;
use warnings;

use Path::Class;
use Carp;
use Scalar::Util qw/blessed/;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/rule _src_lst _tgt_lst _src_pattern _tgt_pattern _need_lst/);

sub new {
	my $self = bless {}, shift;
	local %_ = @_;
	$self->_tgt_lst(ref $_{tgt_lst} eq "ARRAY" ? $_{tgt_lst} : [ $_{tgt_lst} ]) if defined $_{tgt_lst};
	$self->_tgt_pattern($_{tgt_pattern});
	$self->_src_pattern($_{src_pattern});
	$self->_src_lst(ref $_{src_lst} eq "ARRAY" ? $_{src_lst} : [ $_{src_lst} ]) if defined $_{src_lst};
	$self->_need_lst(ref $_{need_lst} eq "ARRAY" ? $_{need_lst} : [ $_{need_lst} ]) if defined $_{need_lst};
    return $self;
}

sub _template_from_pattern($) {
    my $pattern = shift;

    my $template = $pattern;
    if ($template =~ m/%\(.*\)/) {
        $template =~ s/%\((.*)\)/%/;
    }

    return $template;
}

sub _matcher_from_pattern($) {
    my $pattern = shift;

    # TODO Don't descend into . directories?

    my $matcher = $pattern;
    if ($matcher =~ m/%\(.*\)/) {
        $matcher =~ s/%\((.*)\)/($1)/;
    }
    elsif ($matcher =~ m{\%\*}) {
		$matcher =~ s/\%\*/(.*)/;
    }
    elsif ($matcher =~ m{\%\%}) {
		$matcher =~ s/\%/([^\.\/][^\/]*)/;
    }
    elsif ($matcher =~ m{.*/.*\%}) {
		$matcher =~ s/\%/(.*)/;
    }
    else {
		$matcher =~ s/\%/(?:^|\/)([^\.\/][^\/]*)/;
    }

    return qr/$matcher/;
}

sub _name_from($$) {
    my $in = shift;
    my $pattern = shift;
    return $in unless defined $pattern;
    my $matcher = _matcher_from_pattern $pattern;
    my ($out) = $in =~ $matcher;
    return $out;
}

sub _from_name($$) {
    my $name = shift;
    my $pattern = shift;
    return undef unless defined $pattern;
    my $template = _template_from_pattern $pattern;
    my $out = $template;
    $out =~ s/\%/$name/;
    return $out;
}

sub name_from_tgt {
	my $self = shift;
	my $tgt = shift;
	return return _name_from $tgt, $self->_tgt_pattern;
}

sub tgt_from_name {
	my $self = shift;
	my $name = shift;
	return _from_name $name, $self->_tgt_pattern;
}

sub name_from_src {
	my $self = shift;
	my $src = shift;
	return return _name_from $src, $self->_src_pattern;
}

sub src_from_name {
	my $self = shift;
	my $name = shift;
	return _from_name $name, $self->_src_pattern;
}

sub src_matcher {
	my $self = shift;
    return _matcher_from_pattern $self->_src_pattern;
}

sub tgt_lst {
	my $self = shift;

	my @tgt_lst;
	push @tgt_lst, @{ $self->_tgt_lst } if $self->_tgt_lst;

	my @src_lst;
	@src_lst = @{ $self->_src_lst } if $self->_src_lst;
	for my $src (@src_lst) {
        if (blessed $src && $src->isa("Path::Class::Dir") || $src =~ s/~$//) {
            my $src_dir = dir $src;
            next unless -d $src_dir;
            my $src_matcher = $self->src_matcher;
            for ($src_dir->children) {
                next unless $_ =~ $src_matcher;
#               next if $self->src_exclude && $_ =~ $self->src_exclude;
                my $name = $self->name_from_src($_);
                my $tgt = $self->tgt_from_name($name);
                push @tgt_lst, [ $tgt, $_, $self ];
            }
        }
        else {
	        my $name = $self->name_from_src($src);
            my $tgt = $self->tgt_from_name($name);
            push @tgt_lst, $tgt;
        }
	}

	return @tgt_lst;
}

sub can_build {
	my $self = shift;
	my $tgt = shift;
	
	my @tgt_lst = $self->tgt_lst;
	return scalar grep { $tgt eq $_ } @tgt_lst;
}

sub build_all {
	my $self = shift;
	my $rule = shift;

	my @tgt_lst = $self->tgt_lst;

	for my $tgt (@tgt_lst) {
		$rule->build($tgt);
    }
}

package Document::Magick::Maker::Rule;

use strict;
use warnings;

use Path::Class;
use Carp;
use Scalar::Util qw/blessed/;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/maker _map_lst _do label/);

sub new {
	my $self = bless {}, shift;
    my @map_lst;
    if (ref $_[0] eq "HASH") {
        while (@_) {
            last unless ref $_[0] eq "HASH";
            my $map = shift @_;
            push @map_lst, Document::Magick::Maker::Rule::Map->new(%$map);
        }
    }
    elsif (ref $_[0] eq "ARRAY") {
        while (@_) {
            last unless ref $_[0] eq "ARRAY";
            my $map = shift @_;
            push @map_lst, Document::Magick::Maker::Rule::Map->new(
                tgt_lst => $map->[0], tgt_pattern => $map->[1],
                src_pattern => $map->[2], src_lst => $map->[3],
                need_lst => $map->[4],
            );
        }
    }
    else {
	    local %_ = @_;
        push @map_lst, Document::Magick::Maker::Rule::Map->new(
            tgt_lst => delete $_{tgt_lst}, tgt_pattern => delete $_{tgt_pattern},
            src_lst => delete $_{src_lst}, src_pattern => delete $_{src_pattern},
            need_lst => delete $_{need_lst},
        );
    }
    local %_ = @_;
	$self->_map_lst(\@map_lst);
	$self->_do($_{do});
	$self->maker($_{maker});
	$self->label($_{label});
	return $self;
}

sub _find_map_for_tgt {
	my $self = shift;
	my $tgt = shift;

    my @map_lst = @{ $self->_map_lst };
    for my $map (@map_lst) {
        return $map if $map->can_build($tgt);
    }
    return undef;
}

sub build {
	my $self = shift;
	my $tgt = shift;

    if (defined $self->label && $tgt eq $self->label) {
        return $self->build_all;
    }

    my ($src, $name, $map);
	if (ref $tgt eq "ARRAY") {
        # TODO Do we use this?
		($tgt, $src, $map) = @$tgt;

	    $name = $map->name_from_tgt($tgt);
	}
	else {
        $map = $self->_find_map_for_tgt($tgt);

        $name = $map->name_from_tgt($tgt);
        $src = $map->src_from_name($name);
	}

	$tgt = file $tgt;
	$src = file $src if $src;

    if ($src) {
        my $build = 0;

        my @need_lst = @{ $map->_need_lst || [] };
        for my $need (@need_lst) {
	        my $mtime = $self->maker->build($need);
            $build = 1 unless -e $tgt && $tgt->stat->mtime > $mtime;
        }

	    my $mtime = $self->maker->build($src);

#        unless (-e $tgt && $tgt->stat->mtime > (-e $src ? $src->stat->mtime : 0)) {
    
        if ($build || !(-e $tgt && $tgt->stat->mtime > $mtime)) {
            $self->_do->($tgt, $src, $name);
        }

        return $tgt->stat->mtime;
    }
    else {
        $self->_do->($tgt);
        return 0;
    }
}

sub can_build {
	my $self = shift;
	my $tgt = shift;
	
    return 1 if defined $self->label && $tgt eq $self->label;
    return $self->_find_map_for_tgt($tgt);
}

sub build_all {
	my $self = shift;

	my @map_lst = @{ $self->_map_lst };

	for my $map (@map_lst) {
		$map->build_all($self);
    }
}


1;

__END__

package Document::Magick::Maker::Rule;

use strict;
use warnings;

use Path::Class;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/src_dir_lst src_exclude _tgt_lst src_pttn tgt_pttn src_mtch tgt_mtch recipe maker/);

sub mtch_from_pttn($) {
	my $pttn = shift;
	my $mtch = $pttn;
	if ($mtch =~ m/^\//) {
        # TODO Don't descend into . directories?
		$mtch =~ s/\%/([^\.][^\/]*)/;
	}
	else {
        # TODO Don't descend into . directories?
		$mtch =~ s/\%/(.*)/;
	}
	$mtch = qr/$mtch/;
	return $mtch;
}

sub path_from_pttn($$) {
	my $pttn = shift;
	my $name = shift;
	my $path = $pttn;
	$path =~ s/\%/$name/;
	return $path;
}

sub new {
	my $self = bless {}, shift;
	local %_ = @_;
	$self->src_dir_lst($_{src_dir_lst});
	$self->_tgt_lst($_{tgt_lst});
	my $src_pttn = $self->src_pttn($_{src_pttn});
	my $tgt_pttn = $self->tgt_pttn($_{tgt_pttn});
	$self->src_mtch(mtch_from_pttn($src_pttn));
	$self->tgt_mtch(mtch_from_pttn($tgt_pttn));
	$self->recipe($_{recipe});
	$self->maker($_{maker});
	$self->src_exclude($_{src_exclude});
	return $self;
}

sub tgt_name_of {
	my $self = shift;
	my $tgt = shift;
	return unless my ($name) = $tgt =~ $self->tgt_mtch;
	return $name;
}

sub tgt_of {
	my $self = shift;
	my $name = shift;
	return path_from_pttn $self->tgt_pttn, $name;
}

sub src_name_of {
	my $self = shift;
	my $src = shift;
	return unless my ($name) = $src =~ $self->src_mtch;
	return $name;
}

sub src_of {
	my $self = shift;
	my $name = shift;
	return path_from_pttn $self->src_pttn, $name;
}

sub build {
	my $self = shift;
	my $tgt = shift;

	my ($name, $src);

	if (ref $tgt eq "ARRAY") {
		($tgt, $src) = @$tgt;
	}
	else {
		$src = $self->src_of($name);
	}
	$name = $self->tgt_name_of($tgt);

	$tgt = file $tgt;
	$src = file $src;

	$self->maker->build($src);

	unless (-e $tgt && $tgt->stat->mtime > (-e $src ? $src->stat->mtime : 0)) {
		$self->recipe->($tgt, $src, $name);
	}
}

sub can_build {
	my $self = shift;
	my $tgt = shift;
	
	my @tgt_lst = $self->tgt_lst;
	return scalar grep { $tgt eq $_ } @tgt_lst;
}

sub tgt_lst {
	my $self = shift;

	my @tgt_lst;
	push @tgt_lst, @{ $self->_tgt_lst } if $self->_tgt_lst;

	my @src_dir_lst;
	@src_dir_lst = @{ $self->src_dir_lst } if $self->src_dir_lst;
	for (@src_dir_lst) {
		my $dir = dir $_;
		next unless -d $dir;
		my $src_mtch = $self->src_mtch;
		for ($dir->children) {
			next unless $_ =~ $src_mtch;
			next if $self->src_exclude && $_ =~ $self->src_exclude;
			push @tgt_lst, [ $self->tgt_of($self->src_name_of($_)), $_ ];
		}
	}

	return @tgt_lst;
}

sub build_all {
	my $self = shift;

	my @tgt_lst = $self->tgt_lst;

	for my $tgt (@tgt_lst) {
		$self->build($tgt);
    }
}


1;

__END__

sub target_parse {
	my $self = shift;

	my $dir;
	my $target_pttn;

	while (@_) {
		my $value = shift;
		next unless defined $value;

		if ($value =~ m/^~/) {
			my $pttn = $value;
			$dir = undef unless defined $dir;
			$target_pttn = Document::Magick::TargetPattern->new(dir => $dir, pttn => $value);
		}
		else {
			$dir = $value;
		
		}
	}

	return $target_pttn;
}

sub source_parse {
	my $self = shift;

	my @dir_list;
	my @source_pttn;

	while (@_) {
		my $value = shift;
		next unless defined $value;

		if ($value =~ m/^~/) {
			my $pttn = $value;
			unshift @dir_list, undef unless @dir_list;
			push @source_pttn, map { Document::Magick::SourcePattern->new(dir => $_, pttn => $value) } @dir_list;
			undef @dir_list;
		}
		else {
			push @dir_list, $value;
		}
	}

	push @source_pttn, map { Document::Magick::SourcePattern->new(dir => $_, pttn => "~") } @dir_list if @dir_list;

	return @source_pttn;
}

sub parse {
	my $self = shift;

	my (@source_pttn, $target_pttn);
	while (@_) {
		my $value = shift;
		next unless defined $value;

		if ($value eq "-source") {
			$value = shift;
			@source_pttn = $self->source_parse(@$value);
		}
		elsif ($value eq "-target") {
			$value = shift;
			$target_pttn = $self->target_parse(@$value);
		}
		elsif (! @source_pttn) {
			@source_pttn = $self->source_parse(@$value);
		}
		else {
			$target_pttn = $self->target_parse(@$value);
		}
	}

	return ($target_pttn, @source_pttn);
}

package Document::Magick::SourcePattern;

use strict;
use warnings;

use base qw/Class::Accessor::Fast/;

__PACKAGE__->mk_accessors(qw/dir recurse_into_dir pttn/);

sub new {
	my $self = bless {}, shift;
	local %_ = @_;
	my $dir = $_{dir};
	my $recurse_into_dir = $dir =~ s/\*$//;
	my $pttn = $_{pttn};

	$self->dir($dir);
	$self->recurse_into_dir($recurse_into_dir);
	$self->pttn($pttn);

	return $self;
}

package Document::Magick::TargetPattern;

use strict;
use warnings;

sub new {
	my $self = bless {}, shift;
	local %_ = @_;
	my $dir = $_{dir};
	my $pttn = $_{pttn};
	return $self;

	$self->dir($dir);
	$self->pttn($pttn);
}

1;
