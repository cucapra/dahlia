.PHONY: run clean

dev:
	FLASK_APP=buildbot.server FLASK_ENV=development pipenv run flask run

serve:
	pipenv run python -m buildbot.workproc &
	pipenv run gunicorn --bind 0.0.0.0:8000 buildbot.server:app &
	wait

clean:
	rm -rf instance
