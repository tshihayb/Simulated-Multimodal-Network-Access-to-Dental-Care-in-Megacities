# =============================================================================
#  _r13_manuscript_figs_corrected.R -- Figs 2/3/4 rebuilt on the MM-REACH-CORRECTED
#  routing. Reads R13_combos_<tag>_CORRECTED.rds (recovered multimodal trips folded
#  into the Multimodal curves), and per the user's decision DROPS the MC / min-max
#  ribbons (the sweep uncertainty now lives in the R13 break-even tornado). Clean
#  MEAN curves only. Writes Fig_*_CORRECTED.tiff (certified/MCband figs untouched).
#  Standalone from the corrected combos via the _r13_permode.R engine (no pipeline).
# =============================================================================
setwd("C:/Users/Tshih/OneDrive/Claude code projects/Simulation of transit access to dental facilities")
suppressWarnings(suppressMessages({ library(dplyr); library(ggplot2) }))
source("Code/_r13_permode.R")

plasma3 <- viridisLite::plasma(3, end = 0.9)
PAL <- list(
  colour = c("Car-only (direct to clinic)"="grey50", "Metro-only"=plasma3[1], "Multimodal"=plasma3[2]),
  grey   = c("Car-only (direct to clinic)"="grey72", "Metro-only"="grey10",    "Multimodal"="grey52"))  # big shade contrast: metro darkest, multi medium, car light (metro<->multi gap ~42)
amap <- tibble::tribble(~anchor,~Type,~Target,
  "nearest_priv","Private","Nearest","median_priv","Private","Specific","farthest_priv","Private","Farthest","random_priv","Private","Random",
  "nearest_pub","Public","Nearest","median_pub","Public","Specific","farthest_pub","Public","Farthest","random_pub","Public","Random")
TARGET_LV <- c("Nearest","Specific","Farthest","Random")
tlab <- labeller(Target = c(Nearest="Nearest", Specific="Median-distance", Farthest="Farthest", Random="Random"))
tlab2 <- labeller(Target = c(Nearest="Nearest", Specific="Median", Farthest="Farthest", Random="Random"))   # Fig 2: "Median" not "Median-distance"
mode_lab2 <- c("Car-only (direct to clinic)"="Car to facility directly",                                     # Fig 2 mode legend text
               "Metro-only"="Through metro-only",
               "Multimodal"="Through metro and/or bus (multimodal)")
base_theme <- theme_bw(base_size=14) + theme(
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
  axis.line=element_blank(), axis.ticks=element_blank(), legend.position="bottom", legend.box="horizontal",
  plot.title=element_text(face="bold"), plot.subtitle=element_text(size=10), axis.title=element_text(face="bold"),
  legend.title=element_text(face="bold"), strip.text=element_text(face="bold"), strip.background=element_blank(),
  panel.spacing.x=unit(1,"lines"), panel.spacing.y=unit(1.4,"lines"), legend.key.width=unit(1.4,"cm"))

# ---- estimand-consistent evaluator: per-origin mean, then equal weight across origins ----
# Mirrors .r13pm_eval (_r13_permode.R) EXACTLY except for the aggregation stage, which is now
# two-stage: an origin's value is its mean over its eligible draws, and origins then enter the
# curve with equal weight. An origin contributes only if it has >=1 eligible draw for that mode.
# For the nearest/median/farthest anchors there is exactly 1 trip per origin, so this is a
# no-op there (verified: max |delta| 2.7e-12 min on mean_time, 0 on pct/saved).
# _r13_permode.R is deliberately NOT modified: the tornado module shares its pooled evaluator.
.r13pm_eval_origin <- function(fr, P, speeds) {
  af   <- factor(fr$dest_type); ua <- levels(af)
  okey <- factor(paste(fr$dest_type, fr$rp_id, sep="\r"))     # origin within anchor
  oaf  <- factor(sub("\r.*$", "", levels(okey)), levels=ua)   # anchor of each origin-key
  .agg <- function(num, den) {
    sn <- rowsum(num, okey)[,1]; sd <- rowsum(den, okey)[,1]
    mu <- ifelse(sd > 0, sn/sd, NA_real_)                     # per-origin mean over eligible draws
    ok <- is.finite(mu)
    n2 <- rowsum(ifelse(ok, mu, 0), oaf)[,1]
    d2 <- rowsum(as.numeric(ok),    oaf)[,1]
    ifelse(d2 > 0, n2/d2, NA_real_)                           # equal weight per origin
  }
  rows <- list()
  for (s in speeds) {
    car <- fr$road_dist_m/1000/s*60 + P$parking; car0 <- ifelse(is.na(car), 0, car); carok <- as.numeric(!is.na(car))
    rows[[length(rows)+1]] <- data.frame(anchor=ua, Mode_family="Car-only (direct to clinic)", Initiation="Car-initiated",
        speed=s, mean_time=.agg(car0, carok), pct=NA_real_, saved=NA_real_, stringsAsFactors=FALSE)
    for (mode in c("Metro-only","Multimodal")) for (init in c("Car-initiated","Walk-initiated")) {
      t <- .r13pm_time(fr, P, s, if (init=="Car-initiated") "car" else "walk", mode)
      reach <- as.numeric(!is.na(t) & !is.na(car))
      t0    <- ifelse(reach==1, t, 0)
      win   <- ifelse(reach==1 & t < car, 1, 0)
      svnum <- ifelse(win==1, car - t, 0)
      rows[[length(rows)+1]] <- data.frame(anchor=ua, Mode_family=mode, Initiation=init, speed=s,
        mean_time=.agg(t0, reach), pct=100*.agg(win, reach), saved=.agg(svnum, win)/60, stringsAsFactors=FALSE)
    }
  }
  bind_rows(rows)
}

# ---- expand a distinct-(origin,facility) frame to one row per DRAW (per-trip weighting) ----
# R13_combos stores routing ONCE per distinct (origin, facility); Table 1 (from
# sample_test_results, 10 draws/origin) averages over the 10 DRAWS, weighting a facility
# drawn k times by k. This replicates each random-anchor frame row by its draw multiplicity
# (from the anchor bundle) so the figure's within-origin weighting matches Table 1 exactly.
# Only random anchors are expanded (others are 1 draw/origin). Draws to facilities with no
# routing row (same-node / fully transit-unreachable) are carried as NA and excluded from
# every mode's mean, exactly as they are missing in Table 1. Falls back to the distinct frame
# if the bundle or a matching design is unavailable.
.r13pm_expand_perdraw <- function(fr, tag) {
  bf <- "Data/table1_new_anchors_N10.rds"
  if (!file.exists(bf)) { message("[perdraw] bundle missing -> distinct-facility frame kept"); return(fr) }
  bun <- readRDS(bf)
  dsg <- if (tag == "weighted") "Population-weighted" else "Uniform in populated districts only"
  asg <- bun$assignments[[dsg]]
  if (is.null(asg) || is.null(asg$rnd)) { warning("[perdraw] no draws for ", dsg, " -> distinct frame kept"); return(fr) }
  draws <- asg$rnd %>%
    transmute(rp_id = as.character(id), dest_type = as.character(dest_type),
              clinic_id = sub("^(priv_|pub_)", "", as.character(dest_id_geo)))
  fr <- fr %>% mutate(rp_id = as.character(rp_id), dest_type = as.character(dest_type),
                      clinic_id = as.character(clinic_id))
  # The combos metro_only branch is stored PER-TRIP, so .r13pm_frame carries duplicate
  # (origin, facility) rows (identical routing). Collapse to ONE routing row per distinct
  # pair before replicating by the bundle draw count, so each draw maps to exactly one
  # routing record (many-to-one) and a facility drawn k times is weighted exactly k.
  fr_rand <- fr %>% filter(grepl("^random", dest_type)) %>%
    distinct(rp_id, dest_type, clinic_id, .keep_all = TRUE)
  fr_nonr <- fr %>% filter(!grepl("^random", dest_type))
  pd <- draws %>% left_join(fr_rand, by = c("rp_id", "dest_type", "clinic_id"), relationship = "many-to-one")
  nmiss <- sum(is.na(pd$road_dist_m))
  if (nrow(pd) == 0 || nmiss == nrow(pd)) { warning("[perdraw] join produced no matches -> distinct frame kept"); return(fr) }
  message(sprintf("[%s] per-trip expand: random %d distinct facilities -> %d draws (%d draws to same-node/unrouted facilities carried as NA); non-random %d rows",
                  tag, nrow(fr_rand), nrow(pd), nmiss, nrow(fr_nonr)))
  out <- bind_rows(fr_nonr, pd)
  # ---- Align multimodal availability to Table 1 / App Table 2 (results_df) EXACTLY, per anchor ----
  # results_df marks the multimodal chain MISSING for origins whose nearest multimodal access node
  # coincides with the facility's. combos still carries a degenerate route for them, so mask
  # multimodal for every (anchor, origin) that results_df records with no available multimodal trip.
  # This is data-driven from results_df: for the 4 population-weighted origins that sit exactly on a
  # private facility it masks ONLY their nearest-private target (the genuine 0-km loop artifact; App
  # Table 2 footnote b) -- their other targets carry genuine multimodal trips and are left intact.
  # No-op for the uniform sample and for genuinely unreachable origins (already NA in the frame).
  rf <- sprintf("Data/sample_test_results_corrected_%s.rds", tag)
  if (file.exists(rf)) {
    ph <- readRDS(rf) %>%
      group_by(dest_type, rp_id) %>% summarise(has_multi = any(!is.na(multi_total_m)), .groups = "drop") %>%
      filter(!has_multi) %>% mutate(k = paste(as.character(dest_type), as.character(rp_id)))
    mask <- paste(out$dest_type, out$rp_id) %in% ph$k
    pre  <- !is.na(out$mm_l1)
    mm_cols <- grep("^mm_", names(out), value = TRUE)
    out[mask, mm_cols] <- NA
    message(sprintf("[%s] multimodal aligned to results_df availability: %d frame rows flipped to unavailable",
                    tag, sum(mask & pre)))
  }
  out
}

# ---- engine: baseline mean curves only (NO Monte-Carlo band) ----
r13_fig_data_corr <- function(tag, spd=seq(5,80,2.5)) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds", tag)
  if (!file.exists(cf)) stop("corrected combos not found: ", cf, " (run the MM-FULL pass with emit_combos first).")
  combos <- readRDS(cf); if(!"clinic_id"%in%names(combos)) combos$clinic_id<-NA_character_
  # ESTIMAND FIX (2026-07-10): the origin is the unit of analysis. The former
  # `set.seed(7); slice_sample(n=12000)` took a subsample of origin-facility PAIRS, which
  # represented only ~7,245 of the 10,000 random-target origins (27.6% contributed nothing)
  # and weighted the rest by how many of their draws happened to be sampled (1 to 6). ALL
  # random trips are now used, and every anchor is aggregated per origin first.
  combos$metro_pre <- vapply(ifelse(is.na(combos$seg_str),"",combos$seg_str), .pm_metrostr, numeric(1))
  BASE <- r13pm_BASE()
  .fr <- .r13pm_frame(combos, 50, 0.10, "default")
  # PER-TRIP FIX (2026-07-10): match Table 1's within-origin weighting exactly (a facility
  # drawn k times counts k). Expand the random-anchor frame to one row per draw; non-random
  # anchors are unchanged. See .r13pm_expand_perdraw. The car curve is separately overwritten
  # from the per-trip results_df below, so only the metro/multimodal curves change here.
  .fr <- .r13pm_expand_perdraw(.fr, tag)
  base_ev <- .r13pm_eval_origin(.fr, BASE, spd)
  # invariant: 1 trip/origin for the non-random anchors => origin-level == pooled there
  .chk <- .r13pm_eval(.fr, BASE, spd) %>% filter(!grepl("^random", anchor)) %>%
    arrange(anchor, Mode_family, Initiation, speed)
  .ref <- base_ev %>% filter(!grepl("^random", anchor)) %>% arrange(anchor, Mode_family, Initiation, speed)
  message(sprintf("[%s] non-random invariant max|delta| mean_time=%.2e pct=%.2e saved=%.2e",
                  tag, max(abs(.chk$mean_time-.ref$mean_time), na.rm=TRUE),
                  max(abs(.chk$pct-.ref$pct), na.rm=TRUE),
                  max(abs(.chk$saved-.ref$saved), na.rm=TRUE)))
  message(sprintf("[%s] origins contributing per random curve: %s", tag,
    paste(sprintf("%s=%d", c("random_priv","random_pub"),
      vapply(c("random_priv","random_pub"), function(a) dplyr::n_distinct(.fr$rp_id[.fr$dest_type==a]), integer(1))),
      collapse=" ")))
  # ---- Fig-2 CAR curve over ALL random points (direct drive is available to everyone;
  # main-pipeline convention), from the corrected results_df. Transit curves stay over
  # their reachable subsets. Only mean_time is overwritten -> Figs 3 (pct) & 4 (saved),
  # which keep the reachable-only car reference computed inside .r13pm_eval, are untouched.
  rf <- sprintf("Data/sample_test_results_corrected_%s.rds", tag)
  if (file.exists(rf)) {
    # per-origin mean first, then equal weight across origins (road_dist_m is never NA, so the
    # random target is perfectly balanced and this equals the pooled mean exactly; written
    # two-stage anyway so the code states the estimand rather than relying on the balance).
    mroad <- readRDS(rf) %>%
      group_by(dest_type, rp_id) %>% summarise(m = mean(road_dist_m, na.rm = TRUE), .groups = "drop") %>%
      group_by(anchor = dest_type) %>%
      summarise(mroad_km = mean(m, na.rm = TRUE)/1000, .groups = "drop")
    base_ev <- base_ev %>% left_join(mroad, by = "anchor") %>%
      mutate(mean_time = ifelse(Mode_family == "Car-only (direct to clinic)" & is.finite(mroad_km),
                                mroad_km/speed*60 + BASE$parking, mean_time)) %>%
      select(-mroad_km)
    message("[Fig2] car curve set to ALL-points mean (direct-drive population) from ", basename(rf))
  } else message("[Fig2] ", basename(rf), " not found; car curve stays over the reachable frame.")
  fac <- function(x) x %>% mutate(Mode_family=factor(Mode_family,levels=c("Car-only (direct to clinic)","Metro-only","Multimodal")),
    Initiation=factor(Initiation,levels=c("Car-initiated","Walk-initiated")),
    Target=factor(Target,levels=TARGET_LV), Type=factor(Type,levels=c("Private","Public")))
  d  <- base_ev %>% left_join(amap,by="anchor") %>% fac()
  be <- .r13pm_be50(base_ev) %>% left_join(amap,by="anchor") %>% fac()
  list(d=d, be=be)
}

.lt <- scale_linetype_manual(values=c("Car-initiated"="solid","Walk-initiated"="longdash"))
.gl <- guides(color=guide_legend(override.aes=list(linewidth=1.3)), linetype=guide_legend(override.aes=list(linewidth=1.3)))
# Fig-2 "Transit travel initiation" legend key = TWO coloured line segments (metro colour on top,
# multimodal colour below), drawn with the key's linetype (solid=car-initiated / dashed=walk-initiated),
# replacing the default single black key line. Fed by a hidden legend-only geom_line layer in mk_fig2.
.lt_pt <- 72.27/25.4
make_key_twoline <- function(c1, c2) function(data, params, size) {
  lt <- data$linetype; if (is.null(lt) || length(lt)==0 || is.na(lt)) lt <- 1
  grid::gList(
    grid::segmentsGrob(0.05, 0.70, 0.95, 0.70, gp=grid::gpar(col=c1, lwd=1.3*.lt_pt, lty=lt)),
    grid::segmentsGrob(0.05, 0.30, 0.95, 0.30, gp=grid::gpar(col=c2, lwd=1.3*.lt_pt, lty=lt)))
}
# hidden legend-only layer data: two Initiation rows (NA x/y) so the two-colour initiation legend
# renders without drawing anything in the panels. yvar = the y aesthetic column of that figure.
.init_dummy <- function(yvar) {
  d <- data.frame(speed=NA_real_,
                  Initiation=factor(c("Car-initiated","Walk-initiated"), levels=c("Car-initiated","Walk-initiated")))
  d[[yvar]] <- NA_real_; d
}
WT <- function(tag) if (tag=="weighted") "Weighted " else ""
CORR_SUB <- "Multimodal reachability corrected: transfer-rule-valid re-routing; recovered trips folded into the Multimodal curves."
# Multimodal (top line) gets light transparency in COLOUR so metro-only shows through where they overlap; greyscale keeps full opacity (unchanged).
.alpha_mode <- function(grey) c("Car-only (direct to clinic)"=1, "Metro-only"=1, "Multimodal"=if (grey) 1 else 0.65)

mk_fig2 <- function(fd, tag, grey=FALSE) {
  pal <- if(grey) PAL$grey else PAL$colour
  kt  <- make_key_twoline(unname(pal["Metro-only"]), unname(pal["Multimodal"]))   # two-colour initiation legend keys
  dummy <- data.frame(speed=NA_real_, mean_time=NA_real_,
                      Initiation=factor(c("Car-initiated","Walk-initiated"), levels=c("Car-initiated","Walk-initiated")))
  p <- ggplot(fd$d, aes(speed, mean_time/60)) +
    geom_line(aes(color=Mode_family,linetype=Initiation,alpha=Mode_family),linewidth=1.0, show.legend=c(colour=TRUE, linetype=FALSE)) +  # colour (Travel mode) legend only
    geom_line(data=dummy, aes(linetype=Initiation), colour=NA, na.rm=TRUE,
              key_glyph=kt, show.legend=c(colour=FALSE, linetype=TRUE)) +          # hidden layer -> two-colour "Transit travel initiation" legend
    scale_color_manual(values=pal, labels=mode_lab2) + scale_alpha_manual(values=.alpha_mode(grey), guide="none")+.lt+
    scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +
    labs(title=sprintf("Mean Travel Time from %sRandom Point to Dental Facility by Target and Sector",WT(tag)),
         x="Average speed for car/standard bus (km/h)", y="Mean travel time (hours)", color="Travel mode", linetype="Transit travel initiation") +
    base_theme +
    guides(color=guide_legend(order=1, override.aes=list(linewidth=1.3, alpha=1)), linetype=guide_legend(order=2)) +  # Travel mode legend first, then initiation
    theme(axis.ticks = element_line(colour="grey45", linewidth=0.3), axis.ticks.length = unit(2.6,"pt"),
          legend.key.height = unit(0.75,"cm"))  # axes repeated every panel (no gridlines); key height fits the two-line initiation keys
  if (tag == "weighted")   # Private y-scale = Public: fixed 0-8 by 2 for both rows (car peak clipped at 8, not dropped)
    p + facet_grid(Type~Target, scales="fixed", labeller=tlab2, axes="all") +
        scale_y_continuous(breaks=seq(0,8,2)) + coord_cartesian(ylim=c(0,8))
  else
    p + facet_grid(Type~Target, scales="free_y", labeller=tlab2, axes="all")
}
mk_fig3 <- function(fd, tag, grey=FALSE) {
  pal <- if(grey) PAL$grey else PAL$colour
  dd <- fd$d %>% filter(Mode_family!="Car-only (direct to clinic)")
  .splay <- function(z) z %>% group_by(Type, Target) %>% mutate(be_lo=be50==min(be50), be_x=ifelse(be_lo, be50-1.2, be50+1.2), be_h=ifelse(be_lo, 1, 0)) %>% ungroup()   # splay labels outward so close break-evens separate cleanly
  bkc <- fd$be %>% filter(Initiation=="Car-initiated",  is.finite(be50)) %>% .splay()   # car-initiated break-evens: dashed lines, bold labels (upper row)
  bkw <- fd$be %>% filter(Initiation=="Walk-initiated", is.finite(be50)) %>% .splay()   # walk-initiated break-evens: dotted lines, italic labels (lower row)
  ggplot(dd, aes(speed, pct)) +
    geom_hline(yintercept=50, color="grey80", linetype="dotted") +
    geom_vline(data=bkc, aes(xintercept=be50,color=Mode_family),linetype="dashed",linewidth=0.4,alpha=0.6,inherit.aes=FALSE,show.legend=FALSE) +
    geom_vline(data=bkw, aes(xintercept=be50,color=Mode_family),linetype="dotted",linewidth=0.4,alpha=0.55,inherit.aes=FALSE,show.legend=FALSE) +
    geom_text(data=bkc, aes(x=be_x, y=-6,    label=sprintf("%.0f",be50), color=Mode_family, hjust=be_h),
              vjust=0.5, size=3,   fontface="bold",   show.legend=FALSE, inherit.aes=FALSE) +
    geom_text(data=bkw, aes(x=be_x, y=-13, label=sprintf("%.0f",be50), color=Mode_family, hjust=be_h),
              vjust=0.5, size=2.7, fontface="italic", show.legend=FALSE, inherit.aes=FALSE) +   # break-even speed in the margin BELOW the curves, clear of the x tick labels
    geom_line(aes(color=Mode_family,linetype=Initiation,alpha=Mode_family),linewidth=1.0, show.legend=c(colour=TRUE, linetype=FALSE)) +   # colour (Travel mode) legend only
    geom_line(data=.init_dummy("pct"), aes(linetype=Initiation), colour=NA, na.rm=TRUE,
              key_glyph=make_key_twoline(unname(pal["Metro-only"]), unname(pal["Multimodal"])), show.legend=c(colour=FALSE, linetype=TRUE)) +  # two-colour initiation legend
    facet_grid(Type~Target, labeller=tlab2, axes="all") + scale_color_manual(values=pal, labels=mode_lab2) + scale_alpha_manual(values=.alpha_mode(grey), guide="none")+.lt+
    scale_y_continuous(limits=c(-15,100),breaks=seq(0,100,20))+scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +   # negative floor opens a clean strip for the break-even labels (y ticks stay 0-100)
    labs(title=sprintf("Percentage of %sPublic Transit Trips Faster than Direct Driving by Target and Sector",WT(tag)),
         x="Average speed for car/standard bus (km/h)", y="Percentage faster than car (%)", color="Travel mode", linetype="Transit travel initiation") +
    base_theme +
    guides(color=guide_legend(order=1, override.aes=list(linewidth=1.3, alpha=1)), linetype=guide_legend(order=2)) +   # Travel mode legend first, then initiation (two-colour keys)
    theme(axis.ticks = element_line(colour="grey45", linewidth=0.3), axis.ticks.length = unit(2.6,"pt"), legend.key.height = unit(0.75,"cm"))   # Option A: x & y axes repeated on every panel
}
.smooth <- function(x,y){ ok<-is.finite(x)&is.finite(y); if(sum(ok)<5) return(y); fit<-tryCatch(stats::loess(y[ok]~x[ok],span=0.5),error=function(e)NULL); if(is.null(fit)) return(y); out<-rep(NA_real_,length(y)); out[ok]<-stats::predict(fit); out }
mk_fig4 <- function(fd, tag, grey=FALSE, variant="smooth") {
  pal <- if(grey) PAL$grey else PAL$colour
  dd <- fd$d %>% filter(Mode_family!="Car-only (direct to clinic)")
  sub <- "Mean over trips where transit beats driving. Multimodal reachability corrected."
  if (variant=="capped") {
    dd <- dd %>% mutate(saved=ifelse(pct>=2, saved, NA_real_))
    sub <- paste(sub, "Capped where <2% of trips qualify.")
  } else if (variant=="smooth") {
    dd <- dd %>% group_by(anchor,Mode_family,Initiation) %>% arrange(speed) %>% mutate(saved=.smooth(speed,saved)) %>% ungroup()
    sub <- paste(sub, "Loess-smoothed (span 0.5).")
  }
  base <- ggplot(dd, aes(speed, saved)) +
    scale_x_continuous(limits=c(5,80),breaks=seq(10,80,10)) +
    labs(title=sprintf("Mean Time Saved by %sTransit over Direct Driving by Target and Sector",WT(tag)),
         x="Average speed for car/standard bus (km/h)", y="Mean time saved (hours)")
  if (variant=="raw") {   # Fig-2/3 treatment (raw only): relabels, Option A axes, fixed y by 1, no subtitle, two-colour initiation legend
    ytop <- if (tag=="weighted") 6 else 7   # unweighted Nearest-Private peak ~6.8 needs headroom to 7; weighted fits 0-6
    base +
      geom_line(aes(color=Mode_family,linetype=Initiation,alpha=Mode_family),linewidth=1.0,na.rm=TRUE, show.legend=c(colour=TRUE, linetype=FALSE)) +   # colour legend only
      geom_line(data=.init_dummy("saved"), aes(linetype=Initiation), colour=NA, na.rm=TRUE,
                key_glyph=make_key_twoline(unname(pal["Metro-only"]), unname(pal["Multimodal"])), show.legend=c(colour=FALSE, linetype=TRUE)) +  # two-colour initiation legend
      facet_grid(Type~Target, scales="fixed", labeller=tlab2, axes="all") + scale_color_manual(values=pal, labels=mode_lab2) + scale_alpha_manual(values=.alpha_mode(grey), guide="none") + .lt +
      scale_y_continuous(breaks=seq(0,ytop,1)) + coord_cartesian(ylim=c(0,ytop)) +
      labs(color="Travel mode", linetype="Transit travel initiation") + base_theme +
      guides(color=guide_legend(order=1, override.aes=list(linewidth=1.3, alpha=1)), linetype=guide_legend(order=2)) +
      theme(axis.ticks = element_line(colour="grey45", linewidth=0.3), axis.ticks.length = unit(2.6,"pt"), legend.key.height = unit(0.75,"cm"))
  } else   # capped / smooth: unchanged (single black initiation keys)
    base +
      geom_line(aes(color=Mode_family,linetype=Initiation),linewidth=1.0,na.rm=TRUE) +
      facet_grid(Type~Target, scales="free_y", labeller=tlab) + scale_color_manual(values=pal) + .lt +
      labs(subtitle=sub, color="Travel mode", linetype="Transit Initiation") + base_theme + .gl
}

if (Sys.getenv("R13FIG_BUILD") != "0")
for (tag in c("unweighted","weighted")) {
  cf <- sprintf("Data/R13_combos_%s_CORRECTED.rds", tag)
  if (!file.exists(cf)) { cat("SKIP", tag, "- corrected combos not found\n"); next }
  cat("=== building CORRECTED figures:", tag, "===\n"); fd <- r13_fig_data_corr(tag)
  sfx <- if (tag=="weighted") "_weighted" else ""
  sv <- function(base, plt) { ggsave(file.path("Data",paste0(base,".tiff")), plt(FALSE), width=16,height=9,dpi=300,bg="white",compression="lzw")
    if (tag!="weighted") ggsave(file.path("Data",paste0(base,"_grey.tiff")), plt(TRUE), width=16,height=9,dpi=300,bg="white",compression="lzw"); cat("wrote",base,"\n") }
  sv(sprintf("Fig_mean_travel_time%s_ALL8_CORRECTED",sfx),     function(g) mk_fig2(fd,tag,g))
  sv(sprintf("Fig_percent_faster_integrated%s_CORRECTED",sfx), function(g) mk_fig3(fd,tag,g))
  for (v in c("raw","capped","smooth"))
    sv(sprintf("Fig_mean_time_savings%s_CORRECTED_%s",sfx,v),  (function(vv) function(g) mk_fig4(fd,tag,g,vv))(v))
}
cat("DONE\n")
