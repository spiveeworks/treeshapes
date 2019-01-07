function mats = LoadMats4()
  mats(:,:,1) = [1 3 0 0; 1 -1 2 0; 1 -1 -1 1; 1 -1 -1 -1];
  mats(:,:,2) = [1 3 0 0; 1 -1 1 1; 1 -1 1 -1; 1 -1 -2 0];
  mats(:,:,3) = [1 2 1 0; 1 2 -1 0; 1 -2 0 1; 1 -2 0 -1];
  mats(:,:,4) = [1 1 2 0; 1 1 -1 1; 1 1 -1 -1; 1 -3 0 0];
  mats(:,:,5) = [1 1 1 1; 1 1 1 -1; 1 1 -2 0; 1 -3 0 0];
end