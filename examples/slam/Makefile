PROBZELUC=probzeluc

ZLI=$(wildcard *.zli)
ZCI=$(ZLI:zli=zci)

NAME_PF=slam_pf
NAME_DS=slam_ds
NAME_DS_COLOR=slam_ds_color
NAME_RB=slam_rb
NAME_2D=slam_2d
NAME_2D_COLOR=slam_2d_color

all: build

$(NAME_pf).ml main_pf.ml: $(ZCI) $(NAME_PF).zls
	$(PROBZELUC) -s main_pf $(NAME_PF).zls

$(NAME_DS).ml main_ds.ml: $(ZCI) $(NAME_DS).zls
	$(PROBZELUC) -s main_ds $(NAME_DS).zls

$(NAME_DS_COLOR).ml main_ds_color.ml: $(ZCI) $(NAME_DS_COLOR).zls
	$(PROBZELUC) -s main_ds_color $(NAME_DS_COLOR).zls

$(NAME_RB).ml main_rb.ml: $(ZCI) $(NAME_RB).zls
	$(PROBZELUC) -s main_rb $(NAME_RB).zls

$(NAME_2D).ml main_2d.ml: $(ZCI) $(NAME_2D).zls
	$(PROBZELUC) -s main_2d $(NAME_2D).zls

$(NAME_2D_COLOR).ml main_2d_color.ml: $(ZCI) $(NAME_2D_COLOR).zls
	$(PROBZELUC) -s main_2d_color $(NAME_2D_COLOR).zls

clean:
	rm -f $(NAME_PF).ml main_pf.ml
	rm -f $(NAME_DS).ml main_ds.ml
	rm -f $(NAME_DS_COLOR).ml main_ds_color.ml
	rm -f $(NAME_RB).ml main_rb.ml
	rm -f $(NAME_2D).ml main_2d.ml
	rm -f $(NAME_2D_COLOR).ml main_2d_color.ml
	rm -f *.zci

## Generic rules

.phony: build exec

exec: exec_2d

exec_pf: main_pf.ml main_ds.ml main_rb.ml main_2d.ml
	dune exec ./main_pf.exe

exec_ds: main_pf.ml main_ds.ml main_rb.ml main_2d.ml
	dune exec ./main_ds.exe

exec_ds_color: main_pf.ml main_ds.ml main_ds_color.ml main_rb.ml main_2d.ml
	dune exec ./main_ds_color.exe

exec_rb: main_pf.ml main_ds.ml main_rb.ml main_2d.ml
	dune exec ./main_rb.exe

exec_2d: main_pf.ml main_ds.ml main_rb.ml main_2d.ml
	dune exec ./main_2d.exe

exec_2d_color: main_pf.ml main_ds.ml main_2d_color.ml main_rb.ml main_2d.ml
	dune exec ./main_2d_color.exe

build: main_pf.ml main_ds.ml main_rb.ml main_2d.ml main_ds_color.ml main_2d_color.ml
	dune build ./main_pf.exe ./main_ds.exe ./main_rb.exe ./main_ds_color.exe ./main_2d_color.exe

%.zci: %.zli
	$(PROBZELUC) $<

cleanall: clean
	dune clean
	rm -f *~
