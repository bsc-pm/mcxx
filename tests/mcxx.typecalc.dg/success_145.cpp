typedef float CellRenderer;

typedef double A;
typedef wchar_t B;

template<class T_ModelColumnType>
CellRenderer* generate_cellrenderer(char* editable = 0)
{
  return 0;
}

template<>
CellRenderer* generate_cellrenderer<bool>(char* editable);

template<>
CellRenderer* generate_cellrenderer< A >(char* editable);

template<>
CellRenderer* generate_cellrenderer< B >(char* editable);

