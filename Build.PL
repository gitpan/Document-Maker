use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Document::Maker',
    license             => 'perl',
    dist_author         => 'Robert Krimen <rkrimen@cpan.org>',
    dist_version_from   => 'lib/Document/Maker.pm',
    build_requires => {
        'Test::More' => 0,
    },
    requies => {
        'Path::Class' => 0,
        'Moose' => '0.26',
        'Scalar::Util' => 0,
        'Log::Log4perl' => 0,
        'File::Find::Rule' => 0,
        'Carp' => 0,
        'Sub::Exporter' => 0,
    },
    add_to_cleanup      => [ 'Document-Maker-*' ],
);

$builder->create_build_script();
