To compile and execute this example, you need [gym-http-api](https://github.com/openai/gym-http-api), the [openai-gym](https://github.com/openai/gym) REST API and [openai-gym-ocaml](https://github.com/IBM/openai-gym-ocaml), its OCaml binding.

The gym-http-api can be installed as follows:

```
cd ~
git clone https://github.com/openai/gym-http-api
cd gym-http-api
cat << EOF > requirements.txt
Flask==0.11.1
numpy==1.16.3
gym==0.7.4
requests==2.12.4
pytest
EOF
pip install -r requirements.txt
```

The Ocaml binding can be installed as follows:

```
opam install openai-gym
```

To execute this example, you first have to launch the openai-gym:

```
python $PATH_TO_gym-http-api/gym_http_server.py &
```

Then the example can be executed from `~/probzelus/examples/gym-cartpole` with

```
make [exec | exec_smart | exec_pid | exec_smart_pid | exec_simple_pid]
```
