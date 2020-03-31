  %token<int> INT
  %start<unit> file
  %%

  file : with=INT { ignore(with) }
