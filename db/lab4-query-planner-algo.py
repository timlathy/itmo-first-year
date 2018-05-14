value = 106059

# SELECT reltuples
# FROM pg_class
# WHERE relname = 'Н_СЕССИЯ';
# 
# SELECT most_common_vals as mcv, most_common_freqs as mcf,
#        histogram_bounds, null_frac as histogram
# FROM pg_stats
# WHERE tablename = 'Н_СЕССИЯ' AND attname = 'ЧЛВК_ИД';

histogram_selectivity = (1 / len(histogram)) * (histogram.index(value))
histogram_frac = 1 - null_frac - sum(mcf)

mcv_selectivity = sum([f for (v, f) in zip(mcv, mcf) if v < value])
selectivity = mcv_selectivity + (histogram_frac * histogram_selectivity)
rows = reltuples * selectivity
