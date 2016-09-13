// FIXME: jsdoc
export default function cutDirsPath(dirsPath, rootDir) {
  const i = dirsPath.map(d => d.get('id')).indexOf(rootDir.get('id'));
  return (i > -1) ? dirsPath.slice(i) : dirsPath;
}
