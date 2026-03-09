.PHONY: bioc-preflight build check bioccheck-tar bioccheck-git clean-check-artifacts

PKG := monarchr

bioc-preflight:
	$(MAKE) clean-check-artifacts
	$(MAKE) build
	$(MAKE) check
	$(MAKE) bioccheck-tar
	$(MAKE) clean-check-artifacts
	$(MAKE) bioccheck-git
	$(MAKE) clean-check-artifacts

build:
	R CMD build .

check:
	R CMD check --as-cran --no-manual $(PKG)_*.tar.gz

bioccheck-tar:
	R -q -e "BiocCheck::BiocCheck(list.files(pattern='^$(PKG)_.*[.]tar[.]gz$$')[1])"

bioccheck-git:
	R -q -e "BiocCheck::BiocCheckGitClone('.', quitWithStatus=FALSE)"

clean-check-artifacts:
	rm -rf ..Rcheck .Rcheck $(PKG).Rcheck $(PKG).BiocCheck
