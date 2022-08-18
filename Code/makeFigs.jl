using CSV
using DataFrames
using Plots
using Plots.PlotMeasures
using StatsBase
using StatsPlots
using RDatasets
using Distributions
using GaussianMixtures
#plotly()
gr()

properties = [:SepalLength :SepalWidth :PetalLength :PetalWidth]
species = ["versicolor", "virginica", "setosa"]
properties_desc = ["Sepal Length", "Sepal Width", "Petal Length", "Petal Width"]
prop_to_str_desc = Dict(p => s for (p,s) in zip(properties, properties_desc))
hist = []

function Plot3by3HistDecomp(property, iris_df, withFit=false,
                            fitParam=[[0.5, 0.5],
                                      [[0.0]; [5.0];;],
                                      [[0.01]; [0.1];;],
                                      hist, 1])

    α = 1
    if length(fitParam[1]) == 1
        dat = reshape(iris_df[:, property], (nrow(iris_df), 1))
        global dist_mm = fit(Normal, dat)
    else
        my_gmm = GMM(fitParam... )
        dat = reshape(iris_df[:, property], (nrow(iris_df), 1))
        em!(my_gmm, dat, nIter=50)
        global dist_mm = MixtureModel(my_gmm)
    end
    
    vers_vals = iris_df[iris_df[:, "Species"] .== "versicolor", property]
    vers_hist = fit(Histogram,vers_vals, 1:.1:9; closed=:left)
    hist1_plt = hist_plt = histogram(vers_vals,
                                     bins=0:.1:9,
                                     title="Versicolor - $property",
                                     alpha=α, color=1,
                                     legend=false,
                                     ylims=(0,14))

    virg_vals = iris_df[iris_df[:, "Species"] .== "virginica", property]
    virg_hist = fit(Histogram,virg_vals, 1:.1:9; closed=:left)
    hist2_plt = hist_plt = histogram(virg_vals,
                                     bins=0:.1:9,
                                     title="Virgincia - $property",
                                     alpha=α, color=2,
                                     legend=false,
                                     ylims=(0,14))

    seto_val = iris_df[iris_df[:, "Species"] .== "setosa", property]
    seto_hist = fit(Histogram,virg_vals, 1:.1:9; closed=:left)
    hist3_plt = hist_plt = histogram(seto_val,
                                     bins=0:.1:9,
                                     title="Setosa - $property",
                                     alpha=α, color=3,
                                     legend=false,
                                     ylims=(0,14))

    α = 1
    over_hist_plt = hist_plt = histogram(iris_df[iris_df[:, "Species"] .==
                                                 "versicolor", property],
                                         bins=0:.1:9, label="Versicolor", alpha=α, color=1,
                                         title=property)

    over_hist_plt = histogram(over_hist_plt,
                              iris_df[iris_df[:, "Species"] .==
                                      "virginica", property],
                              bins=0:.1:9, label="Virginica", alpha=α, color=2)

    over_hist_plt = histogram(over_hist_plt,
                              iris_df[iris_df[:, "Species"] .==
                                      "setosa", property],
                              bins=0:.1:9, label="Setosa", alpha=α, color=3,
                              ylims=(0,14))
    #@df iris_df
    #Dict("setosa"=> Number(1),
    #     "virginica"=>Number(2),
    #     "versicolor" =>Number(3)))
    over_hist_plt =  groupedhist(iris_df[:,property],
                                 group = iris_df[:,:Species],
                                 bar_position = :stack,
                                 bins=0:.1:9,
                                 title=property, colour=[3 1 2],
                                 ylims=(0,14))

    tot_hist_plt = histogram(iris_df[:,property],
                             bins=0:.1:9, legend=false, alpha=α, color=5,
                             title=property,
                             normalize = withFit,
                             xlabel="(cm)", ylabel="Frequency",
                             ylims=withFit ? :auto : (0,14))
    if withFit
        plot!(tot_hist_plt, dist_mm, legend=false, color=1, fillrange=0.0, fillalpha=.3,
              ylabel="Density")
    end
    
    #plot!(over_hist_plt, title="Cumulative")

    empty_plt = plot(legend=false,grid=false,foreground_color_subplot=:white)

    tot_plt = plot(empty_plt,hist1_plt, empty_plt,
                   tot_hist_plt, hist2_plt, over_hist_plt,
                   empty_plt, hist3_plt,
                   layout=(3,3), size=(1200,800),
                   plot_title="Iris $(prop_to_str_desc[property])",
                   left_margin=30px, bottom_margin=30px)
    #title="Sepal Length",
    return  Dict(:h1 => hist1_plt,
                 :h2 => hist2_plt,
                 :h3 => hist3_plt,
                 :htot => tot_hist_plt,
                 :hover => over_hist_plt,
                 :tot => tot_plt,
                 :dist_mm => dist_mm)
end

function Plot3by2PropVsProp(iris_df)
    empty_plt = plot(legend=false,grid=false,foreground_color_subplot=:white)
    scat_plts = []
    for (i, p1) in enumerate(properties[1:end-1])
        for p2 in properties[i+1:end]
            #series_p1 = iris_df[iris_df[:, "Species"] .== "versicolor", p1]
            #series_p2 = iris_df[iris_df[:, "Species"] .== "versicolor", p2]
            series_p1 = iris_df[:, p1]
            series_p2 = iris_df[:, p2]
            plt = plot(series_p1, series_p2,
                       xlabel="$p1", ylabel="$p2",
                       seriestype = :scatter,
                       legend=false, color=5)
            push!(scat_plts, plt)
        end
    end
    unlabeled_plt = plot(scat_plts..., layout=(2,3), size=(1200,600),
                         left_margin=30px, bottom_margin=30px)
    
    scat_plts = [plot() for i in 1:6]
    plt_count = 1
    for (i, p1) in enumerate(properties[1:end-1])
        for p2 in properties[i+1:end]
            plt = scat_plts[plt_count]
            for (color, spec) in enumerate(species)
                series_p1 = iris_df[iris_df[:, "Species"] .== spec, p1]
                series_p2 = iris_df[iris_df[:, "Species"] .== spec, p2]

                plot!(plt, series_p1, series_p2,
                      xlabel="$p1", ylabel="$p2",
                      seriestype = :scatter,
                      label=spec, color=color,
                      legend=:topleft)
            end
            plt_count += 1
        end
    end
    labeled_plt = plot(scat_plts..., layout=(2,3), size=(1200,600),
                       left_margin=30px, bottom_margin=30px)
    return labeled_plt, unlabeled_plt
end

function Plot3dPropVsProp(iris_df)
    empty_plt = plot(legend=false,grid=false,foreground_color_subplot=:white)
    scat_plts = [plot() for p in 1:3]
    for (p, plt) in zip(properties, scat_plts)
        p_these = setdiff(properties, [p])
        for (color, spec) in enumerate(species)
            series_p1 = iris_df[iris_df[:, "Species"] .== spec, p_these[1]]
            series_p2 = iris_df[iris_df[:, "Species"] .== spec, p_these[2]]
            series_p3 = iris_df[iris_df[:, "Species"] .== spec, p_these[3]]
            plot!(plt, series_p1, series_p2, series_p3,
                  xlabel="$(p_these[1])", ylabel="$(p_these[2])", zlabel="$(p_these[3])",
                  seriestype = :scatter3d,
                  label=spec, color=color, markersize=2)
            
        end
        plot!(plt, title="$p_these") #left_margin=30px
    end
    #plot(scat_plts..., layout=3, size=(1200,600))
    return scat_plts
end

iris_df = dataset("datasets", "iris")

VsPlot = Plot3by2PropVsProp(iris_df)
VsPlot3d = Plot3dPropVsProp(iris_df)

histDist_overlay = [Plot3by3HistDecomp(property, iris_df, true, [[1.0]]) for property in properties]
histDist_overlay_gmm = [Plot3by3HistDecomp(property, iris_df, true)
                        for property in properties]
histDist_overlay_gmm_bad = [Plot3by3HistDecomp(property, iris_df, true,
                                               [[0.5, 0.5],
                                                [[0.0]; [5.0];;],
                                                [[0.1]; [0.1];;],
                                                hist, 1]
                                               )
                            for property in properties]
histDist = [Plot3by3HistDecomp(property, iris_df, false) for property in properties]

savefig(histDist_overlay[2][:htot], "SepalWidthTotHistFit.png")
savefig(histDist[2][:htot], "SepalWidthTotHist.png")

propHist4x1_tot = plot([hd[:htot] for hd in histDist]...,layout=(1,4), size=(1500,400))
savefig(propHist4x1_tot, "propHist4x1_tot.png")

propHist2x1_OverTot = plot([hd[:htot] for hd in histDist_overlay_gmm[3:4]]...,
                           layout=(1,2),
                           size=(900,400), bottom_margin=15px, left_margin=15px,
                           plot_title="Fitted Gaussian Mixed Model")

propHist2x1_OverTot_bad = plot([hd[:htot] for hd in histDist_overlay_gmm_bad[4:4]]...,
                               layout=(1,1),
                               size=(450,400), bottom_margin=15px, left_margin=15px,
                               plot_title="Ill-Fitted Mixed Model")
savefig(propHist2x1_OverTot, "propHist2x1_OverTot.png")
savefig(propHist2x1_OverTot_bad, "propHist2x1_OverTot_bad.png")

savefig(histDist[3][:tot], "histBreakoutPetalLength.png")
savefig(VsPlot[2], "3by2PropVsPropUnLabeled.png")
savefig(VsPlot[1], "3by2PropVsPropLabeled.png")
#[savefig(plt, "$prp.png") for (plt, prp) in zip(fourPlots, properties)]
