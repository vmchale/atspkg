staload "SATS/futhark.sats"
staload "SATS/futhark-arr.sats"
staload "SATS/futhark-stats.sats"

implement main0 () =
  {
    var arr = @[float](1.0f, 2.0f, 3.0f)
    val ctx_cfg = futhark_context_config_new()
    val ctx = futhark_context_new(ctx_cfg)
    val fut_arr = futhark_new_f32_1d(ctx, arr, 3)
    val _ = futhark_free_f32_1d(ctx, fut_arr)
    val () = futhark_context_free(ctx)
    val () = futhark_context_config_free(ctx_cfg)
  }
