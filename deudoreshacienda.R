# Reading and analyzing the Tax Agency (Hacienda) debtors file 2024
# www.overfitting.net
# https://www.overfitting.net/2025/08/analisis-del-fichero-de-morosos-de.html

library(pdftools)  # pdf_info(), pdf_render_page()
library(tesseract)  # tesseract(), ocr()
library(magick)  # image_read(), image_convert(), image_threshold()


# Function that converts a PDF into text by rendering its pages as images
# applying OCR with Tesseract, and extracting only numeric amounts (with commas)
# from each page, skipping the cover
pdf_to_text_ocr <- function(pdf_path, dpi=300) {
    n_pages <- pdf_info(pdf_path)$pages
    texts <- character(n_pages)
    for (i in 2:n_pages) {  # skip cover
        print(paste0("Processing page ", i, "/", n_pages," pages..."))
        
        # Step 1: Convert PDF pages to images
        images <- pdf_render_page(pdf_path, page=i, dpi=dpi)  # returns a raw image
        
        # Step 2: Convert raw image to magick object (if using magick)
        img <- image_read(images)
        
        # Optional: Preprocess image to improve OCR
        img <- image_convert(img, type='Grayscale')
        img <- image_threshold(img, type="white", threshold="70%")

        # Step 3: OCR using tesseract
        engine <- tesseract(options=list(tessedit_pageseg_mode=3))
        texts[i] <- ocr(img, engine=engine)
        
        # Split by newlines
        lines <- unlist(strsplit(texts[i], "\n"))
        # Apply extraction and drop non-numeric results
        amounts <- vapply(lines, extract_amount, character(1), USE.NAMES=FALSE)
        # Keep only those that contain a comma
        amounts <- amounts[grepl(",", amounts)]
        amounts_clean <- amounts[amounts != "" & amounts != ","]
        texts[i]=paste(amounts_clean, collapse = "\n")
    }
    return(paste(texts, collapse="\n"))
}


# Function that scans a string from right to left and extracts a numeric amount
# (digits and at most one comma as a decimal/thousands separator), stopping
# if it encounters any letters
extract_amount <- function(line) {
    # Split the input string into individual characters
    chars <- strsplit(line, "")[[1]]
    # Reverse the characters to scan the string from right to left
    chars <- rev(chars)  
    
    found_comma <- FALSE        # tracks whether a comma has already been included
    result_rev <- character(0)  # stores the extracted characters (in reverse order)
    
    for (ch in chars) {
        # Stop scanning if a letter is encountered
        if (grepl("[A-Za-z]", ch)) break  
        
        # Keep the first comma found (from the right), ignore any additional ones
        if (ch == "," && !found_comma) {
            result_rev <- c(result_rev, ch)
            found_comma <- TRUE
            
        # Keep digits
        } else if (grepl("[0-9]", ch)) {
            result_rev <- c(result_rev, ch)
        }
        # Other characters are skipped
    }
    
    # Reverse the result to restore the original left-to-right order
    result <- paste0(rev(result_rev), collapse = "")
    return(result)
}


###########################

# Read PDF file
# https://sede.agenciatributaria.gob.es/Sede/todas-noticias/2025/junio/27/
# publicacion-listado-deudores-hacienda-publica.html
textos=pdf_to_text_ocr("Listado_deudores.pdf", dpi=500)
lines <- unlist(strsplit(textos, "\n"))
lines <- lines[lines != ""]
writeLines(lines, "Cifras_deudores.csv")


# Drop wrongly read figures (assumption)
MINIMO=600000  # extreme values obtained by visual inspection
MAXIMO=277813329.41  # REYAL URBIS, pág. 98
numeros=as.numeric(gsub(",", ".", lines))
numeros=numeros[numeros >= MINIMO & numeros <= MAXIMO]


# Plot histogram
minimo=min(numeros)
mediana=median(numeros)
media=mean(numeros)
maximo=max(numeros)

numeros2=numeros
numeros2[numeros2>10e6]=10e6  # group >10 million € debtors
png("hist_deudores.png", width=610, height=400)
    hist(numeros2/1000000, breaks=800, xlim=c(0,10), xlab='Debt (million €)',
         border=NA, axes=FALSE, col=rgb(0,0,1),
         main=paste0('Distr. Tax Agency (Hacienda) debtors - 2024\n',
                      '(min=', round(minimo/1e6,1),
                      ', med=', round(mediana/1e6,1),
                      ', avg=', round(media/1e6,1),
                      ', max=', round(maximo/1e6,1),')')
         )
    
    axis(1, at=seq(from=0, to=10, by=1))
    abline(v=c(minimo, mediana, media, maximo)/1e6, col='red', lty=2)
    Pantoja=1009253.55
    Bertin=865601.41
    abline(v=c(Pantoja,Bertin)/1e6)
dev.off()


