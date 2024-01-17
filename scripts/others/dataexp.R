p <- ggplot(WHO) +
  facet_wrap(~iso3, scales = 'free_y') +
  geom_line(aes(x = year, y = e_inc_num), colour = 'salmon') +
  geom_ribbon(aes(x = year, ymin = e_inc_num_lo, ymax = e_inc_num_hi), fill = 'salmon', alpha = 0.2) +
  geom_vline(xintercept = 2000, linetype = 'dashed', colour = 'black')
ggsave("WHO_plot.pdf", plot = p, width = 80, height = 60, units = "cm")