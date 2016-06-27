function build_mex()
  comp('magneetti');
end

function comp(fname)
  fprintf('>> compiling %s\n', fname);
  mex( ['-DFUNC=' fname '_' ], ...
       '-o', fname, '-lmagneetti',...
       ['-L' pwd ], ['-Wl,--rpath=' pwd ],...
       'build_mex.c');
end

