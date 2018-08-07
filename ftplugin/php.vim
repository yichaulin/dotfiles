set shiftwidth=4
set softtabstop=4
set tabstop=4

" 配置 ctags 的参数
let g:gutentags_ctags_extra_args = ['--fields=+aimlS', '--languages=php', '--exclude="\.git"', '--exclude=public', '--exclude=resources', '--exclude=tests', '--exclude=node_modules', '--exclude=config', '--PHP-kinds=+cdfint-av', '--exclude=composer.phar', '--exclude=*Test.php', '--exclude=*phpunit*']
