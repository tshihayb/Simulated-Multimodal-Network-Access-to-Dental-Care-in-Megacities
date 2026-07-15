# =============================================================================
#  _r14_chain_be.R  --  R1-4 EXTENSIONS, sourced + called from the RUN_R14 hook
#  (in-pipeline: needs rp_comparison-type comp_obj, L1_metro_lookup/L1_bus_lookup,
#   the Table-1 assignments .asg, ratio constants, + R13_combos_<tag>.rds on disk,
#   and the _r13_permode.R engine + _r14_imputation.R leg engine).
#
#  (A) L1-CHAIN 3-METHOD: the imputation method (complete-case / ratio / PMM-MI)
#      changes the L1 access leg (imputed ~12.7% metro / ~4% bus), which flows into
#      the Table-2 transit-chain numbers. Reports, per anchor x method, the mean of:
#        - Total metro-only chain (km)      + L1 (RP->Metro) share (%)
#        - Total multimodal chain (km)      + L1 (RP->Access) share (%)
#
#  (B) BREAK-EVEN 50%-CROSSING per method: feed each method's per-trip DIRECT car
#      distance into the per-mode engine, take the speed where 50% of (car-initiated)
#      trips are slower than driving (manuscript Fig-3 definition).
#
#  Reproduction-safe: additive; writes R14_chain_3method_<tag>.{rds,docx} +
#  R14_breakeven50_3method_<tag>.{rds,docx}. Default OFF (only runs in the R14 hook).
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

# ---- L1 native/geo per point from a comparison object (dest_type nearest_metro/bus) ----
.r14_l1_legs <- function(comp_obj) {
  if (is.null(comp_obj) || !all(c("id","dest_type","geo_km","net_km_p2t") %in% names(comp_obj))) return(NULL)
  m <- comp_obj %>% filter(dest_type=="nearest_metro") %>% transmute(rp_id=as.character(id), l1m_net=net_km_p2t, l1m_geo=geo_km)
  b <- comp_obj %>% filter(dest_type=="nearest_bus")   %>% transmute(rp_id=as.character(id), l1b_net=net_km_p2t, l1b_geo=geo_km)
  full_join(m, b, by="rp_id")
}

# ratio-of-means imputer (mirrors the pipeline L1 rule): per stratum r=mean(net)/mean(geo)
.r14_ratio_fill <- function(net, geo) {
  r <- mean(net[is.finite(net)], na.rm=TRUE) / mean(geo[is.finite(net) & is.finite(geo)], na.rm=TRUE)
  if (!is.finite(r)) r <- 1.3
  ifelse(is.finite(net), net, geo*r)
}

# =====================================================================
#  r14_chain_be(tag) — both extensions for one weighting
# =====================================================================
r14_chain_be <- function(tag, out_dir=NULL, corrected=FALSE) {
  ge <- globalenv(); .sfx <- if (isTRUE(corrected)) "_CORRECTED" else ""
  msg <- function(...) message(sprintf("[R1-4-CHAIN %s%s] %s", tag, .sfx, sprintf(...)))
  if (is.null(out_dir)) out_dir <- if (exists("base_dir",envir=ge)) get("base_dir",envir=ge) else getwd()
  cf <- file.path(out_dir, sprintf("R13_combos_%s%s.rds", tag, .sfx))   # corrected => the MM-reach-corrected combos
  if (!file.exists(cf)) { msg("combos %s missing; skip.", basename(cf)); return(invisible(NULL)) }
  if (!exists(".r13pm_frame", envir=ge)) { rc<-file.path(if(exists("base_dir",envir=ge)) file.path(dirname(get("base_dir",envir=ge)),"Code") else "Code","_r13_permode.R"); if(file.exists(rc)) source(rc) }
  # ORIGIN-BASED (Fig-3) evaluator + per-draw expander come from the corrected-figures engine. Source
  # it with the figure build gated OFF and the working directory preserved (it setwd()s and the per-draw
  # expander reads Data/ relatively). This lets the random target use the SAME estimand as Fig 3.
  if (!exists(".r13pm_eval_origin", envir=ge)) {
    .fc <- file.path(if(exists("base_dir",envir=ge)) file.path(dirname(get("base_dir",envir=ge)),"Code") else "Code","_r13_manuscript_figs_corrected.R")
    if (file.exists(.fc)) local({ .owd<-getwd(); on.exit(setwd(.owd)); .ob<-Sys.getenv("R13FIG_BUILD"); Sys.setenv(R13FIG_BUILD="0"); source(.fc); Sys.setenv(R13FIG_BUILD=.ob) })
  }
  combos <- readRDS(cf); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  amap <- c(nearest_priv="Private/Nearest",median_priv="Private/Median",farthest_priv="Private/Farthest",random_priv="Private/Random",
            nearest_pub="Public/Nearest",median_pub="Public/Median",farthest_pub="Public/Farthest",random_pub="Public/Random")

  # comparison object for L1 (same probing as the orchestrate)
  comp_obj <- NULL
  for (nm in (if(tag=="weighted") c("rp_comparison_weighted","rp_comparison_w","weighted_rp_comparison","rp_comparison") else c("rp_comparison")))
    if (is.null(comp_obj) && exists(nm,envir=ge)) { cand<-get(nm,envir=ge); if(all(c("id","dest_type","geo_km","net_km_p2t")%in%names(cand))) comp_obj<-cand }
  l1 <- .r14_l1_legs(comp_obj)
  if (is.null(l1)) { msg("comparison object for L1 not found; L1-chain skipped."); }

  # ================= (A) TRANSIT-CHAIN 3-METHOD =================
  chain_tab <- NULL
  if (!is.null(l1)) {
    # per-point 3-method L1 (metro & bus): CC=native(NA where missing), ratio, PMM(mean of completions)
    l1 <- l1 %>% mutate(
      l1m_cc = l1m_net, l1m_ratio = .r14_ratio_fill(l1m_net, l1m_geo),
      l1b_cc = l1b_net, l1b_ratio = .r14_ratio_fill(l1b_net, l1b_geo))
    pmm_ok <- exists("r14_build_leg_table", envir=ge) && exists("r14_pmm_fill", envir=ge)
    if (pmm_ok) {
      .pf <- function(net,geo){ nn<-length(net); t<-r14_build_leg_table(net,geo,rep(0,nn),rep(0,nn),rep("all",nn),rep("all",nn),rep("All",nn),leg="L1"); p<-tryCatch(r14_pmm_fill(t,m=20,donors=5L),error=function(e)NULL)
        if(is.null(p)||is.null(p$completed)) return(.r14_ratio_fill(net,geo)); rowMeans(sapply(p$completed,function(d) d$net_km)) }
      l1$l1m_pmm <- .pf(l1$l1m_net,l1$l1m_geo); l1$l1b_pmm <- .pf(l1$l1b_net,l1$l1b_geo)
    } else { l1$l1m_pmm <- l1$l1m_ratio; l1$l1b_pmm <- l1$l1b_ratio }

    # Chain from the CERTIFIED best-path route decomposition in the per-draw results (the SAME route as
    # App Table 2 / Figs 2-3): L2/L3 are held fixed and only the imputed L1 access leg swaps by method
    # (ratio = certified L1, so ratio chain == total; complete-case = native L1, NA-dropped; PMM = PMM L1).
    # Aggregate per origin over its draws, then equal weight across origins -- the App-Table-2 estimand
    # (per-draw: a facility drawn k times contributes k times, since the results carry one row per draw).
    .strf <- file.path(out_dir, sprintf("sample_test_results_corrected_%s.rds", tag))
    if (file.exists(.strf)) {
      str <- readRDS(.strf); str$rp_id <- as.character(str$rp_id)
      lj  <- l1 %>% transmute(rp_id, l1m_cc, l1m_pmm, l1b_cc, l1b_pmm)
      .rollup <- function(rows) rows %>% mutate(share=100*L1n/tot) %>%
        group_by(anchor,method,rp_id) %>% summarise(tot=mean(tot,na.rm=TRUE), share=mean(share,na.rm=TRUE), .groups="drop") %>%
        group_by(anchor,method) %>% summarise(km=mean(tot,na.rm=TRUE), L1sh=mean(share,na.rm=TRUE), .groups="drop")
      cm <- str %>% filter(!is.na(metro_only_total_m)) %>%
        transmute(rp_id, anchor=as.character(dest_type), base=metro_only_total_m/1000, L1=metro_L1_m/1000) %>%
        left_join(lj, by="rp_id") %>% mutate(cc=l1m_cc, pm=l1m_pmm)
      chain_metro <- .rollup(bind_rows(
        cm %>% transmute(anchor,rp_id,method="ratio",         tot=base,       L1n=L1),
        cm %>% transmute(anchor,rp_id,method="complete_case", tot=base-L1+cc, L1n=cc),
        cm %>% transmute(anchor,rp_id,method="pmm",           tot=base-L1+pm, L1n=pm))) %>%
        rename(metro_chain_km=km, metro_L1_share=L1sh)
      cx <- str %>% filter(!is.na(multi_total_m)) %>%
        transmute(rp_id, anchor=as.character(dest_type), base=multi_total_m/1000, L1=multi_L1_m/1000, mode=as.character(multi_L1_mode)) %>%
        left_join(lj, by="rp_id") %>%
        mutate(cc=dplyr::case_when(mode=="metro"~l1m_cc, mode=="bus"~l1b_cc, TRUE~L1),
               pm=dplyr::case_when(mode=="metro"~l1m_pmm, mode=="bus"~l1b_pmm, TRUE~L1))
      chain_multi <- .rollup(bind_rows(
        cx %>% transmute(anchor,rp_id,method="ratio",         tot=base,       L1n=L1),
        cx %>% transmute(anchor,rp_id,method="complete_case", tot=base-L1+cc, L1n=cc),
        cx %>% transmute(anchor,rp_id,method="pmm",           tot=base-L1+pm, L1n=pm))) %>%
        rename(multi_chain_km=km, multi_L1_share=L1sh)
      chain_tab <- full_join(chain_metro, chain_multi, by=c("anchor","method")) %>%
        mutate(label=amap[anchor]) %>% arrange(anchor, factor(method,levels=c("complete_case","ratio","pmm")))
      msg("transit-chain 3-method (best-path route, per-draw origin mean; total chain km + L1 share %% per anchor x method):")
      print(as.data.frame(chain_tab %>% mutate(across(where(is.numeric),~round(.x,2)))), row.names=FALSE)
    } else msg("per-draw results %s not found; chain 3-method skipped.", basename(.strf))
  }

  # ================= (B) BREAK-EVEN 50%-CROSSING per method =================
  # Direct-leg native distance per (rp_id,dest_type,clinic_id), from the originals
  # comparison (clinic_id=NA) + the Table-1 assignments (clinic_id from dest_id_geo).
  # ratio road = combos road_dist_m (the certified ratio-imputed primary); CC = native
  # (drop the imputed trips); PMM = native else PMM-mean. Then feed each method's road
  # into the per-mode engine and take the 50%-crossing (Car-initiated).
  be50_tab <- NULL
  N <- if (exists("N_RANDOM_DRAWS",envir=ge)) get("N_RANDOM_DRAWS",envir=ge) else 10
  .t1 <- file.path(out_dir, sprintf("table1_new_anchors_N%d.rds", N))
  .asg <- if (file.exists(.t1)) readRDS(.t1)$assignments[[if(tag=="weighted")"Population-weighted" else "Uniform in populated districts only"]] else NULL
  if (exists(".r13pm_eval", envir=ge) && !is.null(.asg)) {
    ck <- function(g) gsub("^(priv_|pub_)","",g)
    dir_keyed <- bind_rows(
      if (!is.null(comp_obj)) comp_obj %>% filter(dest_type %in% c("nearest_priv","median_priv","nearest_pub","median_pub")) %>%
        transmute(rp_id=as.character(id),dest_type,clinic_id=NA_character_, net=net_km_p2t, geo=geo_km) else NULL,
      .asg$far %>% transmute(rp_id=as.character(id),dest_type,clinic_id=ck(dest_id_geo), net=net_km_p2t, geo=geo_km),
      .asg$rnd %>% transmute(rp_id=as.character(id),dest_type,clinic_id=ck(dest_id_geo), net=net_km_p2t, geo=geo_km)) %>%
      mutate(is_missing = !is.finite(net)) %>%
      # fr (from .r13pm_frame) is UNIQUE per (rp_id,dest_type,clinic_id); random draws that
      # re-pick the same clinic create duplicate keys here -> collapse so the join stays 1:1.
      distinct(rp_id, dest_type, clinic_id, .keep_all = TRUE)
    # PMM mean per row (geo-only predictor; break-even is method-insensitive so this suffices)
    if (exists("r14_build_leg_table",envir=ge) && exists("r14_pmm_fill",envir=ge)) {
      .nn <- nrow(dir_keyed)
      t <- r14_build_leg_table(dir_keyed$net, dir_keyed$geo, rep(0,.nn),rep(0,.nn), ifelse(grepl("priv",dir_keyed$dest_type),"priv","pub"), dir_keyed$dest_type, rep("All",.nn), leg="direct")
      p <- tryCatch(r14_pmm_fill(t, m=20, donors=5L), error=function(e) NULL)
      dir_keyed$pmm <- if (!is.null(p) && !is.null(p$completed)) rowMeans(sapply(p$completed,function(d) d$net_km)) else .r14_ratio_fill(dir_keyed$net,dir_keyed$geo)
    } else dir_keyed$pmm <- .r14_ratio_fill(dir_keyed$net,dir_keyed$geo)

    combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
    fr <- .r13pm_frame(combos, 50, 0.10, "default")
    # per-draw expansion + phantom mask, matching Fig 3 exactly (a facility drawn k times counts k). The
    # expander reads Data/ relatively, so run it from the project root and restore the working directory.
    if (exists(".r13pm_expand_perdraw", envir=ge))
      fr <- local({ .owd<-getwd(); on.exit(setwd(.owd)); setwd(dirname(out_dir)); .r13pm_expand_perdraw(fr, tag) })
    frk <- fr %>% left_join(dir_keyed %>% select(rp_id,dest_type,clinic_id,is_missing,pmm), by=c("rp_id","dest_type","clinic_id"))
    ratio_km <- fr$road_dist_m/1000
    cc_km  <- ifelse(coalesce(frk$is_missing,FALSE), NA_real_, ratio_km)           # drop imputed
    pmm_km <- ifelse(coalesce(frk$is_missing,FALSE), frk$pmm,  ratio_km)           # native else PMM
    msg("[validate] direct-leg imputed share over fr rows = %.2f%% (expect ~4%%); pmm join NA = %d",
        100*mean(coalesce(frk$is_missing,FALSE)), sum(is.na(frk$pmm) & coalesce(frk$is_missing,FALSE)))
    BASE <- r13pm_BASE(); spd <- seq(5,80,2.5)
    .r13eng <- if (exists(".r13pm_eval_origin", envir=ge)) .r13pm_eval_origin else .r13pm_eval   # ORIGIN-BASED (Fig-3)
    be_for <- function(km, md) { fr2<-fr; fr2$road_dist_m<-km*1000
      .r13pm_be50(.r13eng(fr2,BASE,spd)) %>% transmute(anchor,Mode_family,Initiation,method=md,be50) }
    be50_tab <- bind_rows(be_for(cc_km,"complete_case"), be_for(ratio_km,"ratio"), be_for(pmm_km,"pmm")) %>%
      mutate(label=amap[anchor]) %>% arrange(anchor, Mode_family, Initiation, factor(method,levels=c("complete_case","ratio","pmm")))
    msg("break-even 50%%-crossing (both initiations) per anchor x mode x method:")
    print(as.data.frame(be50_tab %>% mutate(be50=round(be50,2))), row.names=FALSE)
  } else msg("engine or Table-1 assignments unavailable; 50%%-crossing break-even skipped.")

  # persist
  out <- list(chain_3method=chain_tab, breakeven50=be50_tab, tag=tag, corrected=isTRUE(corrected))
  saveRDS(out, file.path(out_dir, sprintf("R14_chain_be_%s%s.rds", tag, .sfx))); msg("wrote R14_chain_be_%s%s.rds", tag, .sfx)
  if (requireNamespace("flextable",quietly=TRUE) && requireNamespace("officer",quietly=TRUE)) {
    suppressWarnings(suppressMessages({ library(flextable); library(officer) }))
    .wx <- function(df,path,ttl) if(!is.null(df)&&nrow(df)){ save_as_docx(add_header_lines(autofit(theme_booktabs(flextable(as.data.frame(df %>% mutate(across(where(is.numeric),~round(.x,2))))))), ttl), path=path); msg("wrote %s", basename(path)) }
    .wx(chain_tab, file.path(out_dir,sprintf("R14_chain_3method_%s%s.docx",tag,.sfx)), sprintf("L1-chain imputation 3-method (%s%s): total chain km + L1 share, CC/ratio/PMM", tag, .sfx))
    .wx(be50_tab,  file.path(out_dir,sprintf("R14_breakeven50_%s%s.docx",tag,.sfx)), sprintf("Break-even 50%%-crossing (%s%s, ratio/primary)", tag, .sfx))
  }
  invisible(out)
}
invisible(NULL)
