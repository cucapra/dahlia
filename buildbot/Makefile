.PHONY: run clean

dev:
	FLASK_APP=buildbot FLASK_DEBUG=1 pipenv run flask run

serve:
	pipenv run gunicorn buildbot:app

clean:
	rm -rf instance
