.PHONY: run clean

dev:
	FLASK_APP=buildbot FLASK_DEBUG=1 pipenv run flask run

serve:
	pipenv run gunicorn --bind 0.0.0.0:8000 buildbot:app

clean:
	rm -rf instance
