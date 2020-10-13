function build_mex()
  comp('magnet');
end

function comp(fname)
  fprintf('>> compiling %s\n', fname);
  mex( ['-DFUNC=' fname '_' ], ...
       '-o', fname, '-lmagnet',...
       ['-L' pwd ], ['-Wl,--rpath=' pwd ],...
       'build_mex.c');
end

