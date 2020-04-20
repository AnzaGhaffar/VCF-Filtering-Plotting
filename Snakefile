configfile: "config.yaml"

rule FeatureComplete:
    input:
        expand("chrom_filtered_vcf/chrom_{vcf_file_name}.vcf",vcf_file_name=config["vcf_file"]),
        "Histogram for Quality for Reference Observations.png"



rule Chrom_VCF_Filter:
    input:
        expand("{vcf_file_name}.vcf.gz",vcf_file_name=config["vcf_file"]),
    output:
        expand("chrom_filtered_vcf/chrom_{vcf_file_name}.vcf",vcf_file_name=config["vcf_file"])
    params:
        filter_data=config["vcffilter"]["filters"]
    shell:
        "( bcftools view"
        "   {params.filter_data}"
        "   {input}"
        "   -O v"
        "   -o {output}"
        ")"

rule FeaturePLots:
    input:
        expand("chrom_filtered_vcf/chrom_{vcf_file_name}.vcf",vcf_file_name=config["vcf_file"]),
    output:
        "Histogram for Quality for Reference Observations.png",
    script:
        "Scripts/plotsfeature.R"


