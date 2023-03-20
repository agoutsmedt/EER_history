

noodle_collab <- alluv_dt_as_nodes[EU_US_collab=="USA Only" | EU_US_collab=="Europe Only"]
noodle_collab[,n_collab_cluster:=.N, .(Window, EU_US_collab, Label_com)]
noodle_collab[,n_collab_window:=.N, .(Window, EU_US_collab)]
noodle_collab[,n_cluster_window:=.N, .(Window, Label_com)]
noodle_collab[,n_window:=.N, .(Window)]
noodle_collab[,share_collab_cluster:=n_collab_cluster/n_cluster_window]
noodle_collab[,share_collab_window:=n_collab_window/n_window]

noodle_collab <- noodle_collab[EU_US_collab=="Europe Only", head(.SD,1), .(Window, Label_com)]
noodle_collab[,log_share:=log10(share_collab_cluster/share_collab_window)]

ggplot(noodle_collab[new_Id_com %in% com_to_inspect], aes(x=as.numeric(Window), y=log_share, group=Label_com)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "black") +
  geom_hline(yintercept = 0, size = 1, alpha = 0.4) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) +
  # scale_x_continuous(breaks=c(1980, 1985, 1990, 1995, 2000)) +
  facet_wrap(~ Label_com)

noodle_pub <- alluv_dt_as_nodes
noodle_pub[,n_collab_cluster:=.N, .(Window, Revue , Label_com)]
noodle_pub[,n_collab_window:=.N, .(Window, Revue )]
noodle_pub[,n_cluster_window:=.N, .(Window, Label_com)]
noodle_pub[,n_window:=.N, .(Window)]
noodle_pub[,share_collab_cluster:=n_collab_cluster/n_cluster_window]
noodle_pub[,share_collab_window:=n_collab_window/n_window]

noodle_pub <- noodle_pub[Revue =="EUROPEAN ECONOMIC REVIEW", head(.SD,1), .(Window, Label_com)]
noodle_pub[,log_share:=log10(share_collab_cluster/share_collab_window)]

ggplot(noodle_pub[new_Id_com %in% com_to_inspect], aes(x=as.numeric(Window), y=log_share, group=Label_com)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "black") +
  geom_hline(yintercept = 0, size = 1, alpha = 0.4) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75)) +
  # scale_x_continuous(breaks=c(1980, 1985, 1990, 1995, 2000)) +
  facet_wrap(~ Label_com)



alluv_dt_as_nodes[Revue=="EUROPEAN ECONOMIC REVIEW", EER_bin:=1]
alluv_dt_as_nodes[is.na(EER_bin), EER_bin:=0]
alluv_dt_as_nodes[, EU_US_collab_bin:=99]
alluv_dt_as_nodes[EU_US_collab=="Europe Only", EU_US_collab_bin:=1]
alluv_dt_as_nodes[EU_US_collab=="USA Only", EU_US_collab_bin:=0]



alluv_dt_as_nodes[,mean_EER:=mean(EER_bin),new_Id_com]
alluv_dt_as_nodes[,sd_EER:=sd(EER_bin),new_Id_com]
alluv_dt_as_nodes[,zscore := (EER_bin - mean(EER_bin))/sd(EER_bin),new_Id_com]
alluv_dt_as_nodes[,.N,.(Label_com,zscore)][order(zscore)][!is.na(Label_com)]
alluv_dt_as_nodes[,n_try:=.N,new_Id_com]
alluv_dt_as_nodes[,n_success:=sum(EER_bin),new_Id_com]

binom.test(x, n, p)
alluv_dt_as_nodes[,se:=sd(EER_bin)/sqrt(n_try), new_Id_com]

alluv_dt_as_nodes[,.N,.(Label_com,se )][order(se )][!is.na(Label_com)]


eer_contingency <- table(as.character(alluv_dt_as_nodes$EER_bin),as.character(alluv_dt_as_nodes$new_Id_com))
result <- chisquare(as.data.frame.matrix(eer_contingency), B=99)
result$stand.resid.
eer_resid <- result$stand.resid. %>% as.data.table
eer_resid <- melt(eer_resid) %>% as.data.table
eer_resid <-  merge(eer_resid, community_name, by.x = "variable", by.y = "new_Id_com", all.x = TRUE)

collab_contingency <- table(as.character(alluv_dt_as_nodes$EU_US_collab_bin),as.character(alluv_dt_as_nodes$new_Id_com))
result <- chisquare(as.data.frame.matrix(collab_contingency), B=99)
result$stand.resid.
collab_resid <- result$stand.resid. %>% as.data.table
collab_resid <- melt(collab_resid) %>% as.data.table
collab_resid <-  merge(collab_resid, community_name, by.x = "variable", by.y = "new_Id_com", all.x = TRUE)

plot_resid <- merge(eer_resid[variable %in% com_to_inspect, head(.SD,1), variable], collab_resid[variable %in% com_to_inspect, head(.SD,1), variable], by = "variable", all.x = TRUE)

ggplot(plot_resid, aes(x=-1*value.y, y=-1*value.x, label = Label_com.x)) + 
  geom_point() +
  geom_text_repel(max.overlaps = 100) +
  geom_vline(aes(xintercept = 0), size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), size = 1, alpha = 0.3) +
  theme_classic(base_size = 9) 
  
# En ne gardant que les grosses com
eer_contingency <- table(as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect]$EER_bin),as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect]$new_Id_com))
result <- chisquare(as.data.frame.matrix(eer_contingency), B=99)
result$stand.resid.
eer_resid <- result$stand.resid. %>% as.data.table
eer_resid <- melt(eer_resid) %>% as.data.table
eer_resid <-  merge(eer_resid, community_name, by.x = "variable", by.y = "new_Id_com", all.x = TRUE)

collab_contingency <- table(as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect]$EU_US_collab_bin),as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect]$new_Id_com))
result <- chisquare(as.data.frame.matrix(collab_contingency), B=99)
result$stand.resid.
collab_resid <- result$stand.resid. %>% as.data.table
collab_resid <- melt(collab_resid) %>% as.data.table
collab_resid <-  merge(collab_resid, community_name, by.x = "variable", by.y = "new_Id_com", all.x = TRUE)

plot_resid <- merge(eer_resid[variable %in% com_to_inspect, head(.SD,1), variable], collab_resid[variable %in% com_to_inspect, head(.SD,1), variable], by = "variable", all.x = TRUE)

ggplot(plot_resid, aes(x=-1*value.y, y=-1*value.x, label = Label_com.x)) + 
  geom_point() +
  geom_text_repel(max.overlaps = 100) +
  geom_vline(aes(xintercept = 0), size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), size = 1, alpha = 0.3) +
  theme_classic(base_size = 9) 


# collab_contingency <- table(as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect & (EU_US_collab_bin==1|EU_US_collab_bin==0)]$EU_US_collab_bin),as.character(alluv_dt_as_nodes[new_Id_com %in% com_to_inspect & (EU_US_collab_bin==1|EU_US_collab_bin==0)]$new_Id_com))



