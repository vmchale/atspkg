%{^
#ifdef ATS_MEMALLOC_GCBDW
#undef GC_H
#define GC_THREADS
#endif
%}

#include "prelude/DATS/filebas.dats"
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"
#include "libats/libc/DATS/dirent.dats"
#include "libats/ML/DATS/filebas_dirent.dats"
#include "libats/DATS/athread_posix.dats"
#include "$PATSHOMELOCS/atscntrb-bucs320-divideconquerpar/mylibies.hats"
#include "$PATSHOMELOCS/atscntrb-bucs320-divideconquerpar/mydepies.hats"

#staload $DivideConquer
#staload $DivideConquerPar
#staload FWS = $FWORKSHOP_chanlst

assume input_t0ype = string
assume output_t0ype = int

typedef fworkshop = $FWS.fworkshop

fun as_char(s : string) : charNZ =
  $UN.ptr0_get<charNZ>(string2ptr(s))

fun dir_skipped(dir : string) : bool =
  case+ dir of
    | "." => true
    | ".." => true
    | _ when length(dir) > 0 => as_char(dir) = '.'
    | _ => false

fun DirWalk(fws : fworkshop, fname : string, fopr : cfun(string, int)) :
  int =
  let
    val () = $tempenver(fws)
    val () = $tempenver(fopr)
    
    implement DivideConquer$base_test<> (fname) =
      (test_file_isdir(fname) = 0)
    
    implement DivideConquer$base_solve<> (fname) =
      fopr(fname)
    
    implement DivideConquer$divide<> (dir) =
      (let
        var files = streamize_dirname_fname(dir)
        var files = stream_vt_filter_cloptr<string>( files
                                                   , lam (x) => ~dir_skipped(x)
                                                   )
        var files = stream_vt_map_cloptr<string><string>( files
                                                        , lam (file) =>
                                                            string_append3(dir, "/", file)
                                                        )
      in
        list0_of_list_vt(stream2list_vt(files))
      end)
    
    implement DivideConquer$conquer$combine<> (_, rs) =
      (list0_foldleft<int><int>(rs, 0, lam (res, r) => res + r))
    
    implement DivideConquerPar$fworkshop<> () =
      FWORKSHOP_chanlst(fws)
  in
    DivideConquer$solve<>(fname)
  end

fun wc_line(fname : string) : int =
  let
    fun lines(inp : FILEref) : int =
      let
        var stream = streamize_fileref_line(inp)
        var l = stream_vt_length(stream)
      in
        l
      end
    
    val opt = fileref_open_opt(fname, file_mode_r)
  in
    case+ opt of
      | ~None_vt() => (prerrln!("Cannot open the file: ", fname) ; 0)
      | ~Some_vt (inp) => let
        var nline = lines(inp)
      in
        (fileref_close(inp) ; nline)
      end
  end

#define NCPU 4

implement main0 (argc, argv) =
  {
    var fws = $FWS.fworkshop_create_exn()
    var added = $FWS.fworkshop_add_nworker(fws, NCPU)
    var root = (if argc >= 2 then
      argv[1]
    else
      ".") : string
    var nfile = DirWalk(fws, root, lam (fname) => let
                         var nline = wc_line(fname)
                         val _ = println!(fname, ": ", nline)
                       in
                         nline
                       end)
    val () = println!("Total number of lines:\n    ", nfile)
  }