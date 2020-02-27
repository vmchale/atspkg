#include "share/atspre_staload.hats"

staload "SATS/futhark.sats"
staload "SATS/futhark-arr.sats"
staload "SATS/futhark-stats.sats"
staload "SATS/futhark-linalg.sats"

implement main0 () =
  {
    val arr0 = $arrpsz{float} (1.0f, 2.0f, 3.0f)
    var arr1 = $arrpsz{float} (1.0f, 2.0f, 3.0f)
    val ctx_cfg = futhark_context_config_new()
    val ctx = futhark_context_new(ctx_cfg)
    val fut_arr0 = futhark_new_f32_1d(ctx, arr0, 3)
    val fut_arr1 = futhark_new_f32_1d(ctx, arr1, 3)
    var ret: float
    val _ = futhark_entry_mean_f32(ctx, ret, fut_arr0)
    val () = println!(ret)
    val _ = futhark_entry_dotprod_f32(ctx, ret, fut_arr0, fut_arr1)
    val () = println!(ret)
    val _ = futhark_free_f32_1d(ctx, fut_arr0)
    val _ = futhark_free_f32_1d(ctx, fut_arr1)
    val () = futhark_context_free(ctx)
    val () = futhark_context_config_free(ctx_cfg)
    val () = arrayptr_free (arr0)
    val () = arrayptr_free (arr1)
  }
