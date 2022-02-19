library(magrittr)

# data from https://github.com/DevangThakkar/wordle_archive

words <- readr::read_csv("words.csv") %>% 
    dplyr::mutate(word = stringr::str_to_upper(word))
answers <- readr::read_csv("answers.csv") %>% 
    dplyr::mutate(word = stringr::str_to_upper(word))

## data handling

# 全候補単語
words_chr <- words %>% 
    tidyr::separate(word, 
        into = c("_","chr_1","chr_2","chr_3","chr_4","chr_5"),
        sep = "") %>%
    dplyr::select(-`_`) %>%
    dplyr::mutate(id = dplyr::row_number())

# 出現単語
answers_chr <- answers %>% 
    tidyr::separate(word, 
        into = c("_","chr_1","chr_2","chr_3","chr_4","chr_5"),
        sep = "") %>%
    dplyr::select(-`_`) %>%
    dplyr::mutate(day = dplyr::row_number())

# 未出現単語
notanswers_chr <- words %>% 
    dplyr::anti_join(answers, by = "word")
    tidyr::separate(word, 
        into = c("_","chr_1","chr_2","chr_3","chr_4","chr_5"),
        sep = "") %>%
    dplyr::select(-`_`) %>%
    dplyr::mutate(id = dplyr::row_number())

answers_long_data <- answers_chr %>%
    tidyr::pivot_longer(c(-day,contains("chr"))) %>%
    dplyr::rename(
        alphabet = value
    ) %>%
    dplyr::mutate(appeared = 1) %>%
    tidyr::complete(
        day = day,
        alphabet = LETTERS,
        fill = list(appeared = 0)) %>%
    dplyr::group_by(day,alphabet) %>%
    dplyr::summarise(count = sum(appeared)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(alphabet = factor(alphabet, levels = rev(unique(alphabet))))

notanswers_long_data <- words_chr %>%
    tidyr::pivot_longer(c(-id,contains("chr"))) %>%
    dplyr::rename(
        alphabet = value
    ) %>%
    dplyr::mutate(appeared = 1) %>%
    tidyr::complete(
        id = id,
        alphabet = LETTERS,
        fill = list(appeared = 0)) %>%
    dplyr::group_by(id,alphabet) %>%
    dplyr::summarise(count = sum(appeared)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(alphabet = factor(alphabet, levels = rev(unique(alphabet))))

## alphabet sequence plot

g <- ggplot2::ggplot(
    data = answers_long_data,
    ggplot2::aes(x = day, y = alphabet, fill = count))
g <- g + ggplot2::geom_tile()
g <- g + ggplot2::scale_fill_gradient(low="#FFFFFF", high="#FF0000")
g <- g + ggplot2::theme_bw()
g <- g + ggplot2::ggtitle("アルファベットの頻度の推移")
g1 <- g
g1

## answer levenshtein

observed_levenshtein_data <- answers %>%
    dplyr::mutate(day = dplyr::row_number()) %>%
    dplyr::mutate(previous_word = dplyr::lag(word)) %>%
    dplyr::group_by(word) %>%
    dplyr::mutate(levenshtein = utils::adist(word,previous_word)[1,1])

g <- ggplot2::ggplot(
    data = observed_levenshtein_data,
    ggplot2::aes(x = day, y = levenshtein))
g <- g + ggplot2::geom_line(stat = "identity")
g <- g + ggplot2::geom_smooth()
g <- g + ggplot2::theme_bw()
g <- g + ggplot2::ggtitle("前日の単語とのLevenshtein距離の推移")
g2 <- g

library(patchwork)

g3 <- g1 / g2
g3

## total count plot

answers_sum_data <- answers_long_data %>%
    dplyr::group_by(alphabet) %>%
    dplyr::summarise(answers_count = sum(count))

notanswers_sum_data <- notanswers_long_data %>%
    dplyr::group_by(alphabet) %>%
    dplyr::summarise(notanswers_count = sum(count))

count_data <- dplyr::left_join(
            answers_sum_data, 
            notanswers_sum_data,
            by = "alphabet")

## chisq test

count_table <- count_data %>%
    dplyr::select(-alphabet) %>%
    as.matrix

chisq.test(count_table)

## conringency table

proportion_data <- count_data %>%
    dplyr::mutate(
        answers_proportion = answers_count/sum(answers_count),
        notanswers_proportion = notanswers_count/sum(notanswers_count)
    ) %>%
    dplyr::select(-contains("count"))

proportion_long_data <- proportion_data %>%
        tidyr::pivot_longer(-alphabet) %>%
        dplyr::rename(
            data = name,
            proportion = value
        ) %>%
        dplyr::mutate(percent = proportion*100)

g <- ggplot2::ggplot(
    data = proportion_long_data,
    ggplot2::aes(x = alphabet, y = data, fill = percent))
g <- g + ggplot2::geom_tile()
g <- g + ggplot2::scale_fill_gradient(low="#FFFFFF", high="#FF0000")
g <- g + ggplot2::theme_bw()
g <- g + ggplot2::ggtitle("各アルファベットの出現割合(上段が未出現単語、下段が出現単語)")
g4 <- g
g4

## frequent alphabet

percent_diff_data <- proportion_data %>%
    dplyr::mutate(percent_diff = (answers_proportion - notanswers_proportion)*100) %>%
    dplyr::select(-contains("proportion"))

higher_top5 <- percent_diff_data %>%
    dplyr::arrange(desc(percent_diff)) %>%
    dplyr::slice(1:5) %>%
    dplyr::transmute(
        順位 = 1:5,
        出現しやすい文字 = alphabet,
        出現しやすさ = paste0("+",round(percent_diff,3)," %point")
    )

lower_top5 <- percent_diff_data %>%
    dplyr::arrange(percent_diff) %>%
    dplyr::slice(1:5) %>%
    dplyr::transmute(
        出現にくい文字 = alphabet,
        出現しにくさ = paste0(round(percent_diff,3)," %point")
    )

top5_data <- dplyr::bind_cols(higher_top5,lower_top5)

top5_data %>%
    kableExtra::kbl(caption = "候補単語全体と比べて出現しやすい/しにくい文字Top5") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

## simulated vs observed

data_list <- list()
for(i in 1:1000){
    set.seed(i)
    data_list[[i]] <- words %>%
        dplyr::sample_n(nrow(answers)) %>%
        dplyr::mutate(day = dplyr::row_number()) %>%
        dplyr::mutate(previous_word = dplyr::lag(word)) %>%
        dplyr::group_by(word) %>%
        dplyr::mutate(levenshtein = utils::adist(word,previous_word)[1,1])
}
simulated_levenshtein_count_data <- dplyr::bind_rows(data_list, .id = "iter") %>%
    dplyr::group_by(iter,levenshtein) %>%
    dplyr::summarise(n = dplyr::n())

observed_levenshtein_count_data <- observed_levenshtein_data %>%
    dplyr::group_by(levenshtein) %>%
    dplyr::summarise(observed_n = dplyr::n())

g <- ggplot2::ggplot(
    data = simulated_levenshtein_count_data,
    ggplot2::aes(
        x = levenshtein,
        y = n,
        group = levenshtein))
g <- g + ggplot2::geom_boxplot()
g <- g + ggplot2::geom_point(
    data = observed_levenshtein_count_data,
    ggplot2::aes(
        x = levenshtein,
        y = observed_n),
    col = "red"
)
g <- g + ggplot2::ggtitle("Levenshtein距離のシミュレーション値の分布(箱ひげ図)と観測値(赤点)の比較(iteration=1000)")
g5 <- g
g5
