
# extract just the tables
tables_start_index = str_detect(in_text, pattern='\\<table-wrap ') # find places of tables
if(any(tables_start_index)==FALSE){
  this = data.frame(pmc = pmcid, reason = 'No tables')
  excluded = bind_rows(excluded, this)
  next # skip to next  
}
tables_end_index = str_detect(in_text, pattern='\\</table-wrap\\>') # find places of tables (not footnotes)
if(sum(tables_start_index) != sum(tables_start_index)){cat('Table error.\n')}
indices = cbind(which(tables_start_index), which(tables_end_index))
to_keep = NULL
for (v in 1:nrow(indices)){
  to_keep = c(to_keep, indices[v,1]:indices[v,2])
}
# Write just tables to external document 
to_write = paste(in_text[1], '\n<just-tables>\n',
                 paste(in_text[to_keep], collapse = '\n'),
                 '\n</just-tables>', sep='')
#to_write = str_remove_all(to_write, pattern="<p>||</p>") # remove p, was causing errors - may have consequences!
just_tables_xml = tryCatch(as_xml_document(to_write),
                           error = function(e) print('XML error for tables'))
xml2::write_xml(just_tables_xml, file='web/just_tables.xml', encoding = "UTF-8")
# to here, just p not working

# now read into R if page is available 
webpage = read_xml("web/full.xml", encoding='UTF-8') 
# remove <header> (can get confused with labels)
xml_find_all(webpage, ".//header") %>% xml_remove()

# get table captions
clocations = grep("\\<caption", as.character(inside_tables))
captions = xml_text(inside_tables)[clocations]

## write altered text to external file with encoding 
as_xml = tryCatch(as_xml_document(paste(in_text, collapse = '\n')),
                  error = function(e) print('XML error'))
if(class(as_xml)[1] != 'xml_document'){
  this = data.frame(pmc = pmcid, reason = 'XML error')
  excluded = bind_rows(excluded, this)
  next # skip to next
}
xml2::write_xml(as_xml, file='web/full.xml', encoding = "UTF-8")

## get number of tables and potentially move table number
# work out tables that do and do not have captions
clocations = str_locate_all("\\<caption", string=paste(as.character(just_tables), collapse=''))[[1]][,1]
tlocations = str_locate_all("<table-wrap ", string=paste(as.character(just_tables), collapse=''))[[1]][,1]
tables_with_captions = rep(FALSE, length(tlocations))
for (d in 1:length(clocations)){
  diff = tlocations - clocations[d]
  diff = ifelse(diff<0, diff, -99999)
  tables_with_captions[which(diff == max(diff))] = TRUE
}
tables_with_captions = cumsum(tables_with_captions)
new_table_number = which(tables_with_captions == table_number)


# exclude graphical tables  - does not work! some tables have graphics and data
inside_tables = xml_nodes(just_tables, 'table-wrap') %>% xml_children()
graphics = length(grep("\\<graphic", as.character(inside_tables)))
if(graphics > 0){
  this = data.frame(pmc = pmcid, reason = 'Graphical tables')
  excluded = bind_rows(excluded, this)
  next # skip to next
}