(* Original file: ollvm.0.99/ollvm-0.99/src/ollvm/ollvm_parser.mly *)
 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
  * Copyright (c) 2014 OCamlPro - Julien Sagot <ju.sagot@gmail.com>          *
  *                                                                          *
  * Permission to use, copy, modify, and distribute this software for any    *
  * purpose with or without fee is hereby granted, provided that the above   *
  * copyright notice and this permission notice appear in all copies.        *
  *                                                                          *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
  * }}}                                                                      *)

%{

open Ollvm_ast

(* att type is a workaround to simplify parsing of optionnal keywords in
 * global / function declaration / definition.
 * It is far from what would be ideal since it will allow to parse silly
 * LLVM IR (keywords at the bad place, or function attributes in global
 * variable declaration, ...).
 * It will work as expected with valid LLVM IR. *)

type att =
  | OPT_fn_attr of fn_attr list
  | OPT_align of int
  | OPT_section of string
  | OPT_linkage of linkage
  | OPT_visibility of visibility
  | OPT_thread_local of thread_local_storage
  | OPT_addrspace of int
  | OPT_unnamed_addr
  | OPT_cconv of cconv
  | OPT_dll_storage of dll_storage
  | OPT_externally_initialized
  | OPT_gc of string

let rec get_opt f  = function
  | []       -> None
  | hd :: tl -> match f hd with None -> get_opt f tl | x -> x

let get_fn_attrs l =
  match get_opt (function OPT_fn_attr a -> Some a | _ -> None) l with
  | None  -> []
  | Some l -> l

let get_section =
  get_opt (function OPT_section s -> Some s | _ -> None)

let get_align =
  get_opt (function OPT_align a -> Some a | _ -> None)

let get_linkage =
  get_opt (function OPT_linkage l -> Some l | _ -> None)

let get_visibility =
  get_opt (function OPT_visibility v -> Some v | _ -> None)

let get_addrspace =
  get_opt (function OPT_addrspace i -> Some i | _ -> None)

let get_dll_storage =
  get_opt (function OPT_dll_storage i -> Some i | _ -> None)

let get_cconv =
  get_opt (function OPT_cconv i -> Some i | _ -> None)

let get_thread_local =
  get_opt (function OPT_thread_local x -> Some x | _ -> None)

let get_gc =
  get_opt (function OPT_gc x -> Some x | _ -> None)

let is_unnamed_addr l =
  None <> get_opt (function OPT_unnamed_addr -> Some () | _ -> None) l

let is_externally_initialized l =
  None <> get_opt (function OPT_externally_initialized -> Some () | _ -> None) l

%}

%token<string> GLOBAL LOCAL
%token LPAREN RPAREN LCURLY RCURLY LTLCURLY RCURLYGT LSQUARE RSQUARE LT GT EQ COMMA EOF EOL STAR

%token<string> STRING
%token<int> INTEGER
%token<float> FLOAT
%token KW_NULL KW_UNDEF KW_TRUE KW_FALSE KW_ZEROINITIALIZER

%token<string> LABEL

%token KW_DEFINE KW_DECLARE KW_TARGET KW_DATALAYOUT KW_TRIPLE
%token KW_PRIVATE KW_INTERNAL KW_AVAILABLE_EXTERNALLY KW_LINKONCE KW_WEAK KW_COMMON KW_APPENDING KW_EXTERN_WEAK KW_LINKONCE_ODR KW_WEAK_ODR KW_EXTERNAL KW_DLLIMPORT KW_DLLEXPORT
%token KW_DEFAULT KW_HIDDEN KW_PROTECTED
%token KW_CCC KW_FASTCC KW_COLDCC KW_CC
%token KW_UNNAMED_ADDR
%token KW_TYPE KW_X KW_OPAQUE
%token KW_GLOBAL KW_ADDRSPACE KW_CONSTANT KW_SECTION KW_THREAD_LOCAL KW_LOCALDYNAMIC KW_INITIALEXEC KW_LOCALEXEC KW_EXTERNALLY_INITIALIZED
%token KW_ZEROEXT KW_SIGNEXT KW_INREG KW_BYVAL KW_SRET KW_NOALIAS KW_NOCAPTURE KW_NEST
%token KW_ALIGNSTACK KW_ALWAYSINLINE KW_BUILTIN KW_COLD KW_INLINEHINT KW_JUMPTABLE KW_MINSIZE KW_NAKED KW_NOBUILTIN KW_NODUPLICATE KW_NOIMPLICITFLOAT KW_NOINLINE KW_NONLAZYBIND KW_NOREDZONE KW_NORETURN KW_NOUNWIND KW_OPTNONE KW_OPTSIZE KW_READNONE KW_READONLY KW_RETURNS_TWICE KW_SANITIZE_ADDRESS KW_SANITIZE_MEMORY KW_SANITIZE_THREAD KW_SSP KW_SSPREQ KW_SSPSTRONG KW_UWTABLE KW_DEREFERENCEABLE KW_INALLOCA KW_RETURNED KW_NONNULL
%token KW_ALIGN
%token KW_GC
%token KW_ADD KW_FADD KW_SUB KW_FSUB KW_MUL KW_FMUL KW_UDIV KW_SDIV KW_FDIV KW_UREM KW_SREM KW_FREM KW_SHL KW_LSHR KW_ASHR KW_AND KW_OR KW_XOR KW_ICMP KW_FCMP KW_PHI KW_CALL KW_TRUNC KW_ZEXT KW_SEXT KW_FPTRUNC KW_FPEXT KW_UITOFP KW_SITOFP KW_FPTOUI KW_FPTOSI KW_INTTOPTR KW_PTRTOINT KW_BITCAST KW_SELECT KW_VAARG KW_RET KW_BR KW_SWITCH KW_INDIRECTBR KW_INVOKE KW_RESUME KW_UNREACHABLE KW_ALLOCA KW_LOAD KW_STORE KW_ATOMICCMPXCHG KW_ATOMICRMW KW_FENCE KW_GETELEMENTPTR KW_INBOUNDS KW_EXTRACTELEMENT KW_INSERTELEMENT KW_SHUFFLEVECTOR KW_EXTRACTVALUE KW_INSERTVALUE KW_LANDINGPAD
%token KW_NNAN KW_NINF KW_NSZ KW_ARCP KW_FAST
%token<int> I
%token KW_VOID KW_HALF KW_FLOAT KW_DOUBLE KW_X86_FP80 KW_FP128 KW_PPC_FP128 KW_LABEL KW_METADATA KW_X86_MMX
%token KW_UNWIND KW_TO
%token KW_NUW KW_NSW
%token KW_EXACT
%token KW_EQ KW_NE KW_SGT KW_SGE KW_SLT KW_SLE
%token KW_UGT KW_UGE KW_ULT KW_ULE
%token KW_OEQ KW_OGT KW_OGE KW_OLT KW_OLE KW_ONE KW_ORD KW_UNO KW_UEQ KW_UNE
%token KW_TAIL
%token KW_VOLATILE

%token<string> METADATA_ID
%token<string> METADATA_STRING
%token BANGLCURLY
%token KW_ATTRIBUTES
%token<int> ATTR_GRP_ID

%start<Ollvm_ast.toplevelentries> toplevelentries

%%

(* NB: Will produce parsing error with file not ending with a EOL *)
toplevelentries:
  | EOL* m=terminated(toplevelentry, EOL+)* EOF { m }

toplevelentry:
  | d=definition                        { TLE_Definition d               }
  | d=declaration                       { TLE_Declaration d              }
  | KW_TARGET KW_DATALAYOUT EQ s=STRING { TLE_Datalayout s               }
  | KW_TARGET KW_TRIPLE EQ s=STRING     { TLE_Target s                   }
  | i=LOCAL EQ KW_TYPE t=typ            { TLE_Type_decl (ID_Local i, t) }
  | g=global_decl                       { TLE_Global g                   }
  | i=METADATA_ID EQ m=tle_metadata     { TLE_Metadata (i, m)            }
  | KW_ATTRIBUTES i=ATTR_GRP_ID EQ LCURLY a=fn_attr* RCURLY
                                        { TLE_Attribute_group (i, a)     }

(* metadata are not implemented yet, but are at least (partially) parsed *)
tle_metadata:
  | BANGLCURLY m=separated_list(COMMA, METADATA_ID) RCURLY
   { METADATA_Named m }
  | KW_METADATA m=metadata_node
    { m }

metadata_node:
  | BANGLCURLY m=separated_list(COMMA, metadata_value) RCURLY
    { METADATA_Node m }

metadata_value:
  | tconst                      { METADATA_Const $1  }
  | KW_NULL                     { METADATA_Null      } (* null with no type *)
  | KW_METADATA METADATA_STRING { METADATA_String $2 }
  | KW_METADATA METADATA_ID     { METADATA_Id $2     }
  | KW_METADATA metadata_node   { $2                 }

instr_metadata:
  | METADATA_ID METADATA_ID
  {  }

global_decl:
  | ident=GLOBAL EQ
      attrs=global_attr*
      g_constant=global_is_constant
      g_typ=typ
      g_value=const?
      opt=preceded(COMMA, separated_list(COMMA, global_attr))?
      { let opt = match opt with Some o -> o | None -> [] in
        { g_ident=ID_Global ident;
          g_typ;
          g_constant;
          g_value;

          g_linkage = get_linkage attrs;
          g_visibility = get_visibility attrs;
          g_dll_storage = get_dll_storage attrs;
          g_thread_local = get_thread_local attrs;
          g_unnamed_addr = is_unnamed_addr attrs;
          g_addrspace = get_addrspace attrs;
          g_externally_initialized = is_externally_initialized attrs;
          g_section = get_section opt;
          g_align = get_align opt; } }

global_is_constant:
  | KW_GLOBAL { false }
  | KW_CONSTANT { true }

global_attr:
  | a=linkage                           { OPT_linkage a              }
  | a=visibility                        { OPT_visibility a           }
  | a=dll_storage                       { OPT_dll_storage a          }
  | KW_THREAD_LOCAL LPAREN t=tls RPAREN { OPT_thread_local t         }
  | KW_UNNAMED_ADDR                     { OPT_unnamed_addr           }
  | KW_EXTERNALLY_INITIALIZED           { OPT_externally_initialized }

dll_storage:
  | KW_DLLIMPORT { DLLSTORAGE_Dllimport }
  | KW_DLLEXPORT { DLLSTORAGE_Dllexport }

tls:
  | KW_LOCALDYNAMIC { TLS_Localdynamic }
  | KW_INITIALEXEC  { TLS_Initialexec  }
  | KW_LOCALEXEC    { TLS_Localexec    }

declaration:
  | KW_DECLARE
    dc_ret_attrs=param_attr*
    dc_ret_typ=typ
    name=GLOBAL
    LPAREN dc_args=separated_list(COMMA, dc_arg) RPAREN
    post_attrs=global_attr*
    { {dc_type=TYPE_Function(dc_ret_typ, List.map fst dc_args);
       dc_name=ID_Global name;
       dc_param_attrs=(dc_ret_attrs, List.map snd dc_args);} }

definition:
  | KW_DEFINE
    pre_attrs=df_pre_attr*
    df_ret_attrs=param_attr*
    df_ret_typ=typ
    name=GLOBAL
    LPAREN df_args=separated_list(COMMA, df_arg) RPAREN
    post_attrs=df_post_attr* EOL*
    LCURLY EOL*
    df_blocks=df_blocks
    RCURLY
    { { df_prototype = {
          dc_type = TYPE_Function (df_ret_typ,
                                   List.map (fun x -> fst (fst x)) df_args) ;
          dc_param_attrs = (df_ret_attrs,
                           List.map (fun x -> snd (fst x)) df_args) ;
          dc_name=ID_Global name ; } ;
        df_args=List.map snd df_args;
        df_instrs=df_blocks;

        df_linkage = get_linkage pre_attrs;
        df_visibility = get_visibility pre_attrs;
        df_dll_storage = get_dll_storage pre_attrs;
        df_cconv = get_cconv pre_attrs;
        df_attrs = get_fn_attrs post_attrs;
        df_section = get_section post_attrs;
        df_align = get_align post_attrs;
        df_gc = get_gc post_attrs;
        } }

df_blocks:
  | hd_lbl=terminated(LABEL, EOL+)? hd=terminated(instr, EOL+)+
    tl=pair(terminated(LABEL, EOL+), terminated(instr, EOL+)+)*
  { let hb_lbl=match hd_lbl with Some x -> x | _ -> "" in
    (hb_lbl, hd) :: tl}

df_pre_attr:
  | a=linkage                            { OPT_linkage a     }
  | a=visibility                         { OPT_visibility a  }
  | a=dll_storage                        { OPT_dll_storage a }
  | a=cconv                              { OPT_cconv a       }

df_post_attr:
  | KW_ADDRSPACE LPAREN n=INTEGER RPAREN { OPT_addrspace n   }
  | KW_UNNAMED_ADDR                      { OPT_unnamed_addr  }
  | a=fn_attr+                           { OPT_fn_attr a     }
  (* parser conflict with fn_attr+ looking for a df_post_attr list,
   * because if function attribute a2 follows function attribute a1,
   * it can be seen as [...;[a1];[a2];...] or [...;[a1;a2];...].
   * Shifting to [a1;a2] is the good solution and it is how menhir
   * will handle this conflict. *)
  | s=section                            { OPT_section s     }
                                         (* TODO: comdat *)
  | a=align                              { OPT_align a       }
  | KW_GC a=STRING                       { OPT_gc a          }
                                         (* TODO: prefix *)

linkage:
  | KW_PRIVATE                      { LINKAGE_Private                      }
  | KW_INTERNAL                     { LINKAGE_Internal                     }
  | KW_AVAILABLE_EXTERNALLY         { LINKAGE_Available_externally         }
  | KW_LINKONCE                     { LINKAGE_Linkonce                     }
  | KW_WEAK                         { LINKAGE_Weak                         }
  | KW_COMMON                       { LINKAGE_Common                       }
  | KW_APPENDING                    { LINKAGE_Appending                    }
  | KW_EXTERN_WEAK                  { LINKAGE_Extern_weak                  }
  | KW_LINKONCE_ODR                 { LINKAGE_Linkonce_odr                 }
  | KW_WEAK_ODR                     { LINKAGE_Weak_odr                     }
  | KW_EXTERNAL                     { LINKAGE_External                     }

visibility:
  | KW_DEFAULT   { VISIBILITY_Default   }
  | KW_HIDDEN    { VISIBILITY_Hidden    }
  | KW_PROTECTED { VISIBILITY_Protected }

cconv:
  |KW_CCC{CC_Ccc}|KW_FASTCC{CC_Fastcc}|KW_COLDCC{CC_Coldcc}
  |KW_CC n=INTEGER{CC_Cc n}

typ:
  | n=I                                               { TYPE_I n              }
  | KW_VOID                                           { TYPE_Void             }
  | KW_HALF                                           { TYPE_Half             }
  | KW_FLOAT                                          { TYPE_Float            }
  | KW_DOUBLE                                         { TYPE_Double           }
  | KW_X86_FP80                                       { TYPE_X86_fp80         }
  | KW_FP128                                          { TYPE_Fp128            }
  | KW_PPC_FP128                                      { TYPE_Ppc_fp128        }
  | KW_LABEL                                          { TYPE_Label            }
  | KW_METADATA                                       { TYPE_Metadata         }
  | KW_X86_MMX                                        { TYPE_X86_mmx          }
  | t=typ STAR                                        { TYPE_Pointer t        }
  | LSQUARE n=INTEGER KW_X t=typ RSQUARE              { TYPE_Array (n, t)     }
  | t=typ LPAREN ts=separated_list(COMMA, typ) RPAREN { TYPE_Function (t, ts) }
  | LCURLY ts=separated_list(COMMA, typ) RCURLY       { TYPE_Struct ts        }
  | LTLCURLY ts=separated_list(COMMA, typ) RCURLYGT   { TYPE_Packed_struct ts }
  | KW_OPAQUE                                         { TYPE_Opaque           }
  | LT n=INTEGER KW_X t=typ GT                        { TYPE_Vector (n, t)    }

param_attr:
  | KW_ZEROEXT                   { PARAMATTR_Zeroext           }
  | KW_SIGNEXT                   { PARAMATTR_Signext           }
  | KW_INREG                     { PARAMATTR_Inreg             }
  | KW_BYVAL                     { PARAMATTR_Byval             }
  | KW_INALLOCA                  { PARAMATTR_Inalloca          }
  | KW_SRET                      { PARAMATTR_Sret              }
  | KW_ALIGN n=INTEGER           { PARAMATTR_Align n           }
  | KW_NOALIAS                   { PARAMATTR_Noalias           }
  | KW_NOCAPTURE                 { PARAMATTR_Nocapture         }
  | KW_NEST                      { PARAMATTR_Nest              }
  | KW_RETURNED                  { PARAMATTR_Returned          }
  | KW_NONNULL                   { PARAMATTR_Nonnull           }
  | KW_DEREFERENCEABLE LPAREN n=INTEGER RPAREN
                                 { PARAMATTR_Dereferenceable n }

dc_arg: t=typ p=param_attr*         { (t, p)      }
df_arg: t=typ p=param_attr* i=ident { ((t, p), i) }
call_arg: t=typ i=value             { (t, i)      }

fn_attr:
  | KW_ALIGNSTACK LPAREN p=INTEGER RPAREN { FNATTR_Alignstack p     }
  | KW_ALWAYSINLINE                       { FNATTR_Alwaysinline     }
  | KW_BUILTIN                            { FNATTR_Nobuiltin        }
  | KW_COLD                               { FNATTR_Cold             }
  | KW_INLINEHINT                         { FNATTR_Inlinehint       }
  | KW_JUMPTABLE                          { FNATTR_Jumptable        }
  | KW_MINSIZE                            { FNATTR_Minsize          }
  | KW_NAKED                              { FNATTR_Naked            }
  | KW_NOBUILTIN                          { FNATTR_Nobuiltin        }
  | KW_NODUPLICATE                        { FNATTR_Noduplicate      }
  | KW_NOIMPLICITFLOAT                    { FNATTR_Noimplicitfloat  }
  | KW_NOINLINE                           { FNATTR_Noinline         }
  | KW_NONLAZYBIND                        { FNATTR_Nonlazybind      }
  | KW_NOREDZONE                          { FNATTR_Noredzone        }
  | KW_NORETURN                           { FNATTR_Noreturn         }
  | KW_NOUNWIND                           { FNATTR_Nounwind         }
  | KW_OPTNONE                            { FNATTR_Optnone          }
  | KW_OPTSIZE                            { FNATTR_Optsize          }
  | KW_READNONE                           { FNATTR_Readnone         }
  | KW_READONLY                           { FNATTR_Readonly         }
  | KW_RETURNS_TWICE                      { FNATTR_Returns_twice    }
  | KW_SANITIZE_ADDRESS                   { FNATTR_Sanitize_address }
  | KW_SANITIZE_MEMORY                    { FNATTR_Sanitize_memory  }
  | KW_SANITIZE_THREAD                    { FNATTR_Sanitize_thread  }
  | KW_SSP                                { FNATTR_Ssp              }
  | KW_SSPREQ                             { FNATTR_Sspreq           }
  | KW_SSPSTRONG                          { FNATTR_Sspstrong        }
  | KW_UWTABLE                            { FNATTR_Uwtable          }
  | s=STRING                              { FNATTR_String s         }
  | k=STRING EQ v=STRING                  { FNATTR_Key_value (k, v) }
  | i=ATTR_GRP_ID                         { FNATTR_Attr_grp i       }

align: KW_ALIGN p=INTEGER { p }

section: KW_SECTION s=STRING { s }

ibinop_nuw_nsw_opt: (* may appear with `nuw`/`nsw` keywords *)
  | KW_ADD { fun nuw nsw -> Add (nuw, nsw) }
  | KW_SUB { fun nuw nsw -> Sub (nuw, nsw) }
  | KW_MUL { fun nuw nsw -> Mul (nuw, nsw) }
  | KW_SHL { fun nuw nsw -> Shl (nuw, nsw) }

ibinop_exact_opt: (* may appear with `exact` keyword *)
  | KW_UDIV { fun exact -> UDiv exact }
  | KW_SDIV { fun exact -> SDiv exact }
  | KW_LSHR { fun exact -> LShr exact }
  | KW_ASHR { fun exact -> AShr exact }

ibinop_no_opt: (* can not appear with any keyword *)
  |KW_UREM{URem}|KW_SREM{SRem}|KW_AND{And}|KW_OR{Or}|KW_XOR{Xor}

icmp:
  |KW_EQ{Eq}|KW_NE{Ne}|KW_UGT{Ugt}|KW_UGE{Uge} |KW_ULT{Ult}|KW_ULE{Ule}
  |KW_SGT{Sgt}|KW_SGE{Sge}|KW_SLT{Slt}|KW_SLE{Sle}

fcmp:
  KW_FALSE{False}|KW_OEQ{Oeq}|KW_OGT{Ogt}|KW_OGE{Oge}|KW_OLT{Olt}|KW_OLE{Ole}
  |KW_ONE{One}|KW_ORD{Ord}|KW_UNO{Uno}|KW_UEQ{Ueq}|KW_UGT{Ugt}|KW_UGE{Uge}
  |KW_ULT{Ult}|KW_ULE{Ule}|KW_UNE{Une}|KW_TRUE{True}

conversion:
  |KW_TRUNC{Trunc}|KW_ZEXT{Zext}|KW_SEXT{Sext}|KW_FPTRUNC{Fptrunc}
  |KW_FPEXT{Fpext}|KW_UITOFP{Uitofp}|KW_SITOFP{Sitofp}|KW_FPTOUI{Fptoui}
  |KW_FPTOSI{Fptosi}|KW_INTTOPTR{Inttoptr}|KW_PTRTOINT{Ptrtoint}
  |KW_BITCAST{Bitcast}

ibinop:
  | op=ibinop_nuw_nsw_opt nuw=KW_NUW? nsw=KW_NSW?
    { op (nuw <> None) (nsw <> None) }
  | op=ibinop_exact_opt exact=KW_EXACT? { op (exact <> None) }
  | op=ibinop_no_opt { op }

fbinop:
  KW_FADD{FAdd}|KW_FSUB{FSub}|KW_FMUL{FMul}|KW_FDIV{FDiv}|KW_FREM{FRem}

fast_math:
  KW_NNAN{Nnan}|KW_NINF{Ninf}|KW_NSZ{Nsz}|KW_ARCP{Arcp}|KW_FAST{Fast}

instr:
  | op=ibinop t=typ o1=value COMMA o2=value
    { INSTR_IBinop (op, t, o1, o2) }

  | KW_ICMP op=icmp t=typ o1=value COMMA o2=value
    { INSTR_ICmp (op, t, o1, o2) }

  | op=fbinop f=fast_math* t=typ o1=value COMMA o2=value
    { INSTR_FBinop (op, f, t, o1, o2) }

  | KW_FCMP op=fcmp t=typ o1=value COMMA o2=value
    { INSTR_FCmp (op, t, o1, o2) }

  | c=conversion t1=typ v=value KW_TO t2=typ
    { INSTR_Conversion (c, t1, v, t2) }

  | KW_GETELEMENTPTR ?KW_INBOUNDS ptr=tvalue
    idx=preceded(COMMA, tvalue)*
    { INSTR_GetElementPtr (ptr, idx) }

  | KW_TAIL? KW_CALL cconv? list(param_attr) f=tident
    a=delimited(LPAREN, separated_list(COMMA, call_arg), RPAREN)
    list(fn_attr)
    { INSTR_Call (f, a) }

  | KW_ALLOCA t=typ opt=preceded(COMMA, alloca_opt)?
    { let (n, a) = match opt with Some x -> x | None -> (None, None) in
      INSTR_Alloca (t, n, a) }

  | KW_LOAD vol=KW_VOLATILE? tv=tvalue a=preceded(COMMA, align)?
    { INSTR_Load (vol<>None, tv, a) }

  | KW_PHI t=typ table=separated_nonempty_list(COMMA, phi_table_entry)
    { INSTR_Phi (t, table) }

  | KW_SELECT if_=tvalue COMMA then_=tvalue COMMA else_= tvalue
    { INSTR_Select (if_, then_, else_) }

  | KW_EXTRACTELEMENT vec=tvalue COMMA idx=tvalue
    { INSTR_ExtractElement (vec, idx) }

  | KW_INSERTELEMENT vec=tvalue
    COMMA new_el=tvalue COMMA idx=tvalue
    { INSTR_InsertElement (vec, new_el, idx)  }

  | KW_EXTRACTVALUE tv=tvalue COMMA
    idx=separated_nonempty_list (COMMA, INTEGER)
    { INSTR_ExtractValue (tv, idx) }

  | KW_INSERTVALUE agg=tvalue COMMA new_val=tvalue COMMA
    idx=separated_nonempty_list (COMMA, INTEGER)
    { INSTR_InsertValue (agg, new_val, idx) }

  | KW_SHUFFLEVECTOR v1=tvalue COMMA v2=tvalue COMMA mask=tvalue
    { INSTR_ShuffleVector (v1, v2, mask)  }

  | KW_VAARG  { failwith"INSTR_VAArg"  }
  | KW_LANDINGPAD    { failwith"INSTR_LandingPad"    }

  | KW_STORE vol=KW_VOLATILE? all=tvalue COMMA ptr=tident
    a=preceded(COMMA, align)?
    { INSTR_Store (vol<>None, all, ptr, a) }

  | KW_ATOMICCMPXCHG { failwith"INSTR_AtomicCmpXchg" }
  | KW_ATOMICRMW     { failwith"INSTR_AtomicRMW"     }
  | KW_FENCE         { failwith"INSTR_Fence"         }

  | KW_RET t=typ o=value
    { INSTR_Ret (t, o) }

  | KW_RET KW_VOID
    { INSTR_Ret_void }

  | KW_BR c=tvalue COMMA o1=tident COMMA o2=tident
    { INSTR_Br (c, o1, o2) }

  | KW_BR b=tident
    { INSTR_Br_1 b }

  | KW_SWITCH c=tvalue COMMA
    def=tident LSQUARE EOL? table=list(switch_table_entry) RSQUARE
    { INSTR_Switch (c, def, table) }

  | KW_INDIRECTBR tv=tvalue
    COMMA LSQUARE til=separated_list(COMMA, tident)  RSQUARE
    { INSTR_IndirectBr (tv, til) }

  | KW_RESUME tv=tvalue
    { INSTR_Resume tv }

  | KW_UNREACHABLE
    { INSTR_Unreachable }

  | KW_INVOKE cconv? ret=tident
    LPAREN a=separated_list(COMMA, call_arg) RPAREN
    list(fn_attr) KW_TO l1=tident KW_UNWIND l2=tident
    { INSTR_Invoke (ret, a, l1, l2)  }

  | i=ident EQ inst=instr { INSTR_Assign (i, inst) }

alloca_opt:
  | a=align                             { (None, Some a) }
  | nb=tvalue a=preceded(COMMA, align)? { (Some nb, a) }

phi_table_entry:
  | LSQUARE v=value COMMA l=ident RSQUARE { (v, l) }

switch_table_entry:
  | v=tvalue COMMA i=tident EOL? { (v, i) }

const:
  | i=INTEGER                                         { VALUE_Integer i        }
  | f=FLOAT                                           { VALUE_Float f          }
  | KW_TRUE                                           { VALUE_Bool true        }
  | KW_FALSE                                          { VALUE_Bool false       }
  | KW_NULL                                           { VALUE_Null             }
  | KW_UNDEF                                          { VALUE_Undef            }
  | KW_ZEROINITIALIZER                                { VALUE_Zero_initializer }
  | LCURLY l=separated_list(COMMA, tconst) RCURLY     { VALUE_Struct l         }
  | LTLCURLY l=separated_list(COMMA, tconst) RCURLYGT { VALUE_Struct l         }
  | LSQUARE l=separated_list(COMMA, tconst) RSQUARE   { VALUE_Array l          }
  | LT l=separated_list(COMMA, tconst) GT             { VALUE_Vector l         }

value:
  | c=const { c             }
  | i=ident { VALUE_Ident i }

ident:
  | l=GLOBAL { ID_Global l }
  | l=LOCAL  { ID_Local l  }

tvalue: t=typ v=value { (t, v) }
tconst: t=typ c=const { (t, c) }
tident: t=typ i=ident { (t, i) }
