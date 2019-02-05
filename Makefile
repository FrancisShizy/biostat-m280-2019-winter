hw2sol: ./hw2/hwsol.Rmd
    Rscript -e 'rmarkdown::render("$<")'
    
clean:
    rm -rf *.html *.md