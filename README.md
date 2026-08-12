# Eclipse

Realtime state of the 12 August 2026 total solar eclipse over the European sky,
live at <http://erykciepiela.xyz/eclipse/>.

A [Bambik](https://github.com/restaumatic/bambik) application: one profunctor
pipeline ticking every second — a load action reads the wall clock and timezone,
an SVG sun/moon renders the eclipse geometry as seen from the selected place,
and a clickable table of European places shows each one's local circumstances
(contact times, current and maximum obscuration).

Times and obscurations are approximate, interpolated around published contact
times.

## Development

Requires Linux x86_64, node ≥ 18 and network access — the forked PureScript
compiler, the bambik library and the patched variant library all resolve on
first install/build (see `package.json` and `packages.dhall`).

```sh
npm install
npm run watch    # incremental rebuild on each .purs edit
npm run dev      # serve http://127.0.0.1:8000
```

## Deployment

```sh
npm run deploy   # bundle + scp to erykciepiela.xyz
```
